import std/[
  sequtils, 
  lists, 
  strformat, 
  algorithm, 
  tables, 
  options]

import print


using 
  content: string
  slice  : Slice[int]
  str    : string
  ch     : char
  chars  : set[char]
  cursor : int
  mask   : var seq[bool]

type 
  MdSpan = ref object
    tokens: seq[string]
    maskInside: bool

  PatternNode = ref object
    lookup: Table[char, PatternNode]
    span: MdSpan

const notfound = -1

func mds(tks: seq[string], mi: int = 0): MdSpan =
  MdSpan(tokens: tks, maskInside: mi == 1)
  

func indices(smth: string or seq): Slice[int] = 
  0 ..< smth.len

func `[]`[T](s: Slice[T]; cursor: T): T = 
  s.a + cursor

func at(content; cursor): char = 
  if cursor in content.indices: content[cursor]
  else: '\0'  

func at(content; slice; mask; cursor): char = 
  if cursor in slice and not mask[cursor]: content[cursor]
  else: '\0'  

func at[T](s: seq[T]; cursor): T = 
  if cursor in s.indices: s[cursor]
  else: default T


proc findNotEscapedAfter(content; slice; mask; pattern: string): int = 
  for i in slice:
    let j = i-1
    if j in slice and content.at(j) != '\\':
      var matched = true
      for k in pattern.indices:
        if content[i+k] != pattern[k]:
          matched = false
          break
      
      if matched:
        return i + pattern.len

  notfound


proc select[T](s: seq[T], indices: seq[int]): seq[T] = 
  for i in indices:
    result.add s[i]

proc makePatternTree(spanTokens: seq[MdSpan], h = 0): PatternNode = 
  new result

  if spanTokens.len == 1 and spanTokens[0].tokens[0].len == h:
    result.span = spanTokens[0]

  else:
    var acc: Table[char, seq[int]]

    for i, p in spanTokens:
      let t = p.tokens[0]
      if h < t.len:
        let c = t[h]
        if c notin acc:
          acc[c] = @[]
        acc[c].add i

    for c, s in acc:
      result.lookup[c] = makePatternTree(select(spanTokens, s), h+1)

proc match(content; slice; mask; patternTree: PatternNode): MdSpan = 
  var i = slice.a
  var k = 0
  var pt = patternTree

  while i+k in slice:
    let j = i+k
    let ch = at(content,slice, mask, j)

    if ch in pt.lookup:
      pt = pt.lookup[ch]
      inc k
    
    elif not isNil pt.span:
      return pt.span

    else:
      return nil


proc findFirst(content; slice; mask; patternTree: PatternNode): Option[tuple[cursor: int, pn: MdSpan]] = 
  for i in slice:
    let j = i-1
    if content.at(j) != '\\':
      let m = match(content, i .. slice.b, mask, patternTree)
      if not isnil m:
        return some (i, m)

proc bake(content; slice; mask; spanTokens: seq[MdSpan]): seq[tuple[p: MdSpan, borders: seq[Slice[int]]]] = 
  let pt = makePatternTree(spanTokens)

  while true:
    
    let qr = findFirst(content, slice, mask, pt)

    if issome qr:
      let 
        span = qr.get.pn
        tks = span.tokens
        i = qr.get.cursor
        j = findNotEscapedAfter(
          content, 
          i + tks[0].len .. slice.b, 
          mask, 
          tks[^1])

      if j == notfound:
        raise newException(ValueError, fmt"cannot match against {tks}")
      else:
        let 
          b1 = i ..< i+tks[0].len
          b2 = j - tks[^1].len ..< j
          inside = b1.b+1 .. b2.a-1
          borders = @[b1, b2]

        # echo (content[inside], content[b1], content[b2])
        result.add (span, borders)

        # --- mask out
        for b in borders:
          for i in b:
            mask[i] = true

        if span.maskInside:
          for i in inside:
            mask[i] = true

    else:
      break

# -------------------------------------------------

proc bakery(content; slice; mask): seq[tuple[kind: int, slice: Slice[int]]] = 
  let 
    pat1 = @[
      mds(@["`"], 1), 
      mds(@["$"], 1),
      mds(@[ "[[", "]]"], 1),
    ]

    pat2 = @[
      mds(@["(", ")"]),
      mds(@["[", "]"]),
    ]

    pat3 = @[
      mds(@["**"]), 
      mds(@["=="]),
    ]

    pat4 = @[
      mds(@["_"]),
    ]

  # mds(@["// ", "\0"]), # TODO means EOL i.e. end of line
  discard bake(content, slice, mask, pat1)
  discard bake(content, slice, mask, pat2)
  discard bake(content, slice, mask, pat3)
  discard bake(content, slice, mask, pat4)

  # dir and text ...


let str1 = """hello **my [^12] ![img](img.png) $var$ `++` ==name==** is _vahid_"""

# let str2 = """hi [[hey]] `$` $`$ """
var mask2  = newSeqWith(str1.len, false)

echo str1
echo bakery(str1, str1.indices, mask2)
