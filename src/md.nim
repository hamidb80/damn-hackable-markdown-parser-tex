# ----- Imports  ---------------------------------

import std/[
  strutils, strformat, 
  lists, sequtils,
  algorithm,
  options,
]

# ----- Type Defs  -------------------------------

type
  MdNodeKind* = enum
    # wrapper
    mdWrap # XXX add indent to this (for indent detection, like in lists)

    # metadata
    mdFrontMatter ## yaml

    # blocks
    mdbHeader ## ##+ ...
    mdbPar 
    mdbCode ## ``` 
    mdbMath ## $$
    mdbQuote ## > 
    mdbList ## + - *
    mdbTable

    # spans (inline elements)
    mdsDir ## direction of language of choice, consecutive words with whitespace between, emojis and other should be lesf as is
    mdsBoldItalic ## ***...***
    mdsBold ## **...**
    mdsItalic ## *...* _..._
    mdsHighlight ## ==...==
    mdsCode ## `...`
    mdsMath ## $...$
    mdsLink ## []()
    mdsEmbed ## ![]()
    mdsWikilink ## [[ ... ]]
    mdsComment ## // ...
    mdWikiEmbed ## like photos, PDFs, etc ![[...]]
    mdsText ## processed text
    # mdsTag ## #...

    # other
    mdHLine # ---
  
  MdDir* = enum
    mddUndecided
    mddRtl
    mddLtr

  MdNode* = ref object
    # common
    kind:     MdNodeKind
    children: seq[MdNode]
    content:  string
    slice:    Slice[int]

    # specific
    numbered: bool   # for list
    dir:      MdDir  # for text
    priority: int    # for header
    lang:     string # for code
    href:     string # for link
    size:     Option[int]

  MdSettings* = object
    pageWidth*: int

type
  SimplePatternMeta = enum
    spmWhitespace  # \s
    spmDigit       # \d

  SimplePatternTokenKind = enum
    sptChar
    sptMeta

  SimplePatternToken = object
    case kind: SimplePatternTokenKind
    of sptChar:
      ch: char
    of sptMeta:
      meta: SimplePatternMeta

  SimplePatternNode = object
    token: SimplePatternToken
    repeat: Slice[int] = 1..1 # 1 (default), *, +, custom

  SimplePattern = seq[SimplePatternNode]

# ----- Constants --------------------------------

const notfound* = -1

const MdLeafNodes* = {mdsText, 
                      mdsMath, mdsCode, 
                      mdsEmbed, mdWikiEmbed, mdsWikilink, 
                      mdbMath, mdbCode}

# ----- Syntax Sugar -------------------------------

template TODO: untyped =
  raise newException(ValueError, "TODO")

# ----- General Utils ------------------------------

func isUnicode(ch: char): bool = 
  127 < ch.uint

# func `+`(n,m: Slice[int]): Slice[int] = 
#   (n.a + m.a) .. (n.b + m.b)

# func `-`(n,m: Slice[int]): Slice[int] = 
#   (n.a - m.a) .. (n.b - m.b)

# func isEmpty(a: seq): bool = a.len == 0

func at*(str: string, index: int): char = 
  if index in str.low .. str.high: str[index]
  else:                            '\0'

func subtract*(n, m: Slice[int]): seq[Slice[int]] = 
  # case 1
  # n-----------n
  #    m----m
  # o-o      o--o

  # case 2
  #    n-----n
  # m----------m
  #    nothing
  
  # case 3
  # n-----n
  #    m------m
  # o-o

  # case 4
  #    n------n
  # m------m
  #         o-o

  # case 5
  # n------n
  #          m------m
  # o------o

  # case 6
  #          n------n
  # m------m
  #          o------o

  if n.a < m.a: # start only
    result.add n.a .. min(n.b, m.a-1)
  
  if n.b > m.b: # end only
    result.add max(n.a, m.b+1) .. n.b

func intersects*(n, m: Slice[int]): bool =
  subtract(n, m) != @[n]

func contains*(n, m: Slice[int]): bool =
  m.a in n and 
  m.b in n

# ----- Convertors ---------------------------------

func toXml*(n: MdNode, result: var string) = 
  result.add "<"
  result.add $n.kind
  result.add ">"

  case n.kind
  of MdLeafNodes: result.add n.content
  else          : discard

  for i, sub in n.children:
    if i != 0: result.add ' '
    toXml sub, result

  result.add "</"
  result.add $n.kind
  result.add ">"

func toXml*(n: MdNode): string = 
  toXml n, result


func toTex*(n: MdNode, settings: MdSettings, result: var string) = 
  case n.kind

  of mdWrap:
    for i, sub in n.children:
      toTex sub, settings, result
      result.add "\n\n"
  
  of mdbHeader:
    let tag = 
      case n.priority
      of 1: "section"
      of 2: "subsection"
      of 3: "subsubsection"
      else: "par"

    result.add '\\'
    result.add tag
    result.add '{'
    for i, sub in n.children:
      if i != 0: result.add ' '
      toTex sub, settings, result
    result.add '}'

  of mdbPar:
    for i, sub in n.children:
      if i != 0: result.add ' '
      toTex sub, settings, result

  of mdsDir: 
    # \usepackage{bidi}
    # \lr : ltr 
    # \rl : rtl

    if n.dir == mddLtr:
      result.add "\\lr{"

    for i, sub in n.children:
      if i != 0: result.add ' '
      toTex sub, settings, result
  
    if n.dir == mddLtr:
      result.add '}'
 
  of mdsCode: 
    result.add "\\texttt{"
    result.add n.content
    result.add '}'

  of mdsMath: 
    result.add "\\("
    result.add n.content
    result.add "\\)"

  of mdbMath: 
    result.add "\\[\n"
    result.add n.content
    result.add "\n\\]"

  of mdbCode:
    result.add "\\begin{verbatim}\n"
    result.add n.content
    result.add "\n\\end{verbatim}"

  of mdsBoldItalic: 
    result.add "\\verb{"
    for i, sub in n.children:
      if i != 0: result.add ' '
      toTex sub, settings, result
    result.add "}"

  of mdsBold: 
    result.add "\\textbf{"
    for i, sub in n.children:
      if i != 0: result.add ' '
      toTex sub, settings, result
    result.add "}"

  of mdsItalic: 
    result.add "\\textit{"
    for i, sub in n.children:
      if i != 0: result.add ' '
      toTex sub, settings, result
    result.add "}"

  of mdsHighlight: 
    result.add "\\hl{"
    for i, sub in n.children:
      if i != 0: result.add ' '
      toTex sub, settings, result
    result.add "}"

  of mdsComment:
    result.add "\\begin{small}"
    for i, sub in n.children:
      if i != 0: result.add ' '
      toTex sub, settings, result
    result.add "\\end{small}"

  of mdsText: 
    result.add n.content

  of mdHLine: 
    result.add "\\clearpage"

  of mdsWikilink: 
    toTex MdNode(kind: mdsItalic, children: @[MdNode(kind: mdbPar, children: @[
      MdNode(kind: mdsText, content: "WIKILINK")
    ])]), settings, result

  of mdWikiEmbed:
    result.add "\\begin{figure}[H]\n"
    result.add "\\centering\n"
    result.add "\\includegraphics["
    if isSome n.size: 
      let size = (n.size.get / settings.pageWidth) * (15)
      result.add "width="
      result.add formatFloat(size, precision=3)
      result.add "cm,"
    result.add "keepaspectratio]{"
    result.add n.content
    result.add "}\n"
    result.add "\\caption{"
    for i, sub in n.children:
      if i != 0: result.add ' '
      toTex sub, settings, result
    result.add "}\n"
    result.add "\\end{figure}"

  of mdsEmbed:
    TODO

  of mdFrontMatter:
    discard

  of mdbList:
    let tag = 
      if n.numbered: "enumerate"
      else:          "itemize"
    result.add "\\begin{"
    result.add tag
    result.add "}"
    for i, sub in n.children:
      result.add "\n\\item "
      toTex sub, settings, result
    result.add "\n\\end{"
    result.add tag
    result.add "}"

  of mdsLink:
    result.add "\\href{"
    result.add n.content
    result.add "}"

    if n.children.len > 0:
      result.add "{"
      for i, sub in n.children:
        if i != 0: result.add ' '
        toTex sub, settings, result
      result.add "}"

  of mdbQuote:
    TODO

  of mdbTable:
    raise newException(ValueError, fmt"toTex for kind {n.kind} is not implemented")

func toTex*(n: MdNode, settings: MdSettings): string = 
  toTex n, settings, result

# ----- Slice Masking Utils ------------------------

proc replace*[T](list: var DoublyLinkedList[T], n: DoublyLinkedNode[T], left, right: T) = 
  var 
    l = newDoublyLinkedNode(left)
    r = newDoublyLinkedNode(right)

  l.prev = n.prev
  r.next = n.next
  l.next = r
  r.prev = l

  if isNil n.prev:   list.head = l
  else:            n.prev.next = l

  if isNil n.next:   list.tail = r
  else:            n.next.prev = r

proc replace*[T](list: var DoublyLinkedList[T], n: DoublyLinkedNode[T], repl: T) = 
  n.value = repl

proc subtract*(ns: var DoublyLinkedList[Slice[int]], n: DoublyLinkedNode[Slice[int]], m: Slice[int]) = 
  let subs = subtract(n.value, m)
  case subs.len
  of 0: ns.remove n
  of 1: replace(ns, n, subs[0])
  of 2: replace(ns, n, subs[0], subs[1])
  else: raise newException(ValueError, "invalid subs: " & $subs.len)

# ----- Matching Utils -----------------------------

proc p*(pattern: string): SimplePattern = 
  var i = 0
  while i < pattern.len:
    
    let lastToken = 
      case pattern.at(i)
      of '\\':
        inc i
        case pattern.at(i)
        of 's':        SimplePatternToken(kind: sptMeta, meta: spmWhitespace)
        of 'd':        SimplePatternToken(kind: sptMeta, meta: spmDigit)
        of '\\', '+':  SimplePatternToken(kind: sptChar, ch: pattern[i])
        else:    raise newException(ValueError, fmt"invalid meta character '{pattern.at(i+1)}'")
      else:     
          SimplePatternToken(kind: sptChar, ch: pattern[i])

    let repeat = 
      case pattern.at(i+1)
      # of '^':  0 .. 0
      # of '*':  0 .. int.high
      of '+':  1 .. int.high
      else  :  1 .. 1

    result.add SimplePatternNode(token: lastToken, repeat: repeat)

    if 1 != len repeat:
      inc i, 2
    else:
      inc i 

const listPatterns = [p"- ", p"\+ ", p"* ", p "\\d+. "]

proc matches*(ch: char, pt: SimplePatternToken): bool = 
  case pt.kind
  of sptChar: ch == pt.ch
  of sptMeta:
    case pt.meta
    of spmWhitespace: ch in Whitespace
    of spmDigit     : ch in Digits

proc find*(content: string, slice: Slice[int], sub: string): int = 
  var i = slice.a
  var j = 0 
  
  while i in slice:
    if content[i] == sub[j]:
      inc j
      if j == sub.len: return i-j+1
    else:
      dec i, j
      reset j

    inc i

  notfound

proc skipWhitespaces*(content: string, cursor: int): int = # : SkipWhitespaceReport = 
  var i = cursor
  while i < content.len:
    if content[i] in Whitespace:
      inc i
    else:
      break
  i

proc startsWith*(str: string, cursor: int, pattern: SimplePattern): int = 
  if str.high < cursor: return notfound

  var 
    j = 0      # current pattern
    c = 0      # count
    i = cursor # string index

  while true:
    if j == pattern.len: # all sub patterns satisfied
      return i

    if i >= str.len: 
        break

    if matches(str[i], pattern[j].token):
      inc c
      inc i

      if c == pattern[j].repeat.b:
        inc j
        reset c

    elif c in pattern[j].repeat:
        c = 0
        inc j

    else:
      return notfound
  
  i

proc skipBefore*(content: string, cursor: int, pattern: SimplePattern): int = 
  var i  = cursor
  
  while i < content.len:
    if startsWith(content, i, pattern) != notfound: return i-1
    inc i
  
  raise newException(ValueError, "cannot match end of " & $pattern)

proc stripSlice*(content: string, slice: Slice[int], chars: set[char]): Slice[int] = 
  var i = slice.a
  var j = slice.b

  while content[i] in chars: inc i
  while content[j] in chars: dec j
  
  i .. j

proc skipChars*(content: string, slice: Slice[int], chars: set[char]): int = 
  var i = slice.a
  while i in slice:
    if content[i] notin chars: break
    inc i
  i # or at the end of file

proc skipChar*(content: string, slice: Slice[int], ch: char): int = 
  skipChars(content, slice, {ch})

proc skipNotChar*(content: string, slice: Slice[int], ch: char): int = 
  var i = slice.a
  while i in slice:
    if content[i] == ch: break
    inc i
  i # or at the end of file

proc skipAtNextLine*(content: string, slice: Slice[int]): int = 
  skipNotChar(content, slice, '\n')


proc scrabbleMatchDeep*(content: string, indexes: var DoublyLinkedList[Slice[int]], pattern: string): Option[Slice[int]] =
  var j = 0
  var n: DoublyLinkedNode[Slice[int]]

  block findPattern:
    for ni in indexes.nodes:
      let area = ni.value # consequtive indexes
      for i in area:
        let cond = (i == 0 or content[i-1] != '\\') and # considers escape
                    pattern[j] == content[i]
        if cond:
          inc j
          if j == pattern.len: 
            result = some i-j+1 .. i
            n = ni
            break findPattern

        else:
          reset j

  # no change the indexes
  if not isNil n:
    subtract indexes, n, result.get

proc scrabbleMatchDeepMulti*(content: string, indexes: var DoublyLinkedList[Slice[int]], pattern: seq[string]): Option[seq[Slice[int]]] = 
  var acc: seq[Slice[int]]

  # TODO do not to manipulate indexes here
  for p in pattern:
    let match = scrabbleMatchDeep(content, indexes, p)
    if issome match:
      acc.add match.get
    else:
      break

  if acc.len == pattern.len:
    return some acc

  elif acc.len == pattern.len - 1:
    raise newException(ValueError, "cannot match")
  
  else:
    discard

# ----- Main Functionalities ---------------------

proc detectBlockKind*(content: string, cursor: int): MdNodeKind = 
  if   startsWith(content, cursor, p"```")   != notfound: mdbCode
  elif startsWith(content, cursor, p"$$\s")  != notfound: mdbMath
  elif startsWith(content, cursor, p"> ")    != notfound: mdbQuote
  elif startsWith(content, cursor, p"#+ ")   != notfound: mdbHeader
  elif startsWith(content, cursor, p"---+")  != notfound: mdHLine
  elif startsWith(content, cursor, p"![[")   != notfound: mdWikiEmbed
  elif startsWith(content, cursor, p"![")    != notfound: mdsEmbed
  elif listPatterns.anyit(notfound != startsWith(content, cursor, it)): mdbList
  else: mdbPar

proc skipAfterParagraphSep*(content: string, slice: Slice[int], kind: MdNodeKind): int = 
  ## go until double \s+\n\s+\n

  var newlines = 0
  var i = slice.a

  while i in slice:
    case content[i]
    of '\n':                
      inc newlines
      if detectBlockKind(content, i+1) != kind: break # there is one exception and that is if there be a list just after the paragraph or $$ or ``` or ![[ or ---- :(

    of Whitespace - {'\n'}: discard
    elif 2 <= newlines:     break
    else:                   reset newlines
    inc i
  
  i

proc afterBlock*(content: string, cursor: int, kind: MdNodeKind): int = 
  case kind
  of mdbHeader: skipAtNextLine(content, cursor .. content.high)
  of mdHLine:   skipAtNextLine(content, cursor .. content.high)

  of mdbPar:    skipAfterParagraphSep(content, cursor .. content.high, mdbPar)
  of mdbQuote:  skipAfterParagraphSep(content, cursor .. content.high, mdbPar)
  of mdbList:   skipAfterParagraphSep(content, cursor .. content.high, mdbList)


  of mdWikiEmbed:
    let pat = "]]"
    let i = cursor + len "![["
    let e = skipBefore(content, i, p pat)
    1 + e + len pat

  of mdsEmbed:
    # FIXME
    let pat = ")"
    let i = cursor + len "!["
    let e = skipBefore(content, i, p pat)
    1 + e + len pat

  of mdbCode: 
    let pat = "\n```"
    let i = cursor + len "```"
    let e = skipBefore(content, i, p pat)
    1 + e + len pat
  
  of mdbMath: 
    let pat = "\n$$"
    let i = cursor + len "$$"
    let e = skipBefore(content, i, p pat)
    1 + e + len pat
  

  else: 
    raise newException(ValueError, fmt"invalid block type '{kind}'")

proc onlyContent*(content: string, slice: Slice[int], kind: MdNodeKind): Slice[int] = 
  case kind
  of mdbMath:      stripSlice(content, slice, {'$'} + Whitespace)
  of mdbCode:      stripSlice(content, slice, {'`'})
  of mdbHeader:    stripSlice(content, slice, {'#'} + Whitespace)
  of mdbQuote:     stripSlice(content, slice, {'>'} + Whitespace)
  of mdbPar:       stripSlice(content, slice, Whitespace)
  of mdWikiEmbed:  stripSlice(content, slice, {'!', '[', ']'} + Whitespace)
  else: slice

proc detectLang(content: string, area: Slice[int]): MdDir =
  for i in area:
    if isUnicode    content[i]: return mddRtl
    if isAlphaAscii content[i]: return mddLtr
  return mddUndecided


func empty(z: seq): bool = 
  z.len == 0

func filled(z: seq): bool = 
  not empty z

proc wordSlices(content: string, area: Slice[int]): seq[Slice[int]] =
  var changes: seq[int]
  var l = true # last was whitespace?

  for i in area:
    let w = content[i] in Whitespace
    if  w != l:
      changes.add i
    l = w

  if changes.filled and not l:
    changes.add area.b+1

  for i in countup(1, changes.high, 2):
    let head = changes[i-1]
    let tail = changes[i]-1
    result.add head..tail

proc meltSeq(elements: seq[MdDir]): seq[Slice[int]] = 
  var j = 0

  for i in 1 ..< elements.len:
    if   elements[i] == mddUndecided:
      discard

    elif elements[j] != elements[i]:
      result.add j ..< i
      j = i

  let s = j ..< elements.len
  if 1 <= len s:
    result.add s

proc separateLangs(content: string, area: Slice[int]): seq[MdNode] =
  let 
    ws        = wordSlices(content, area)
    langs     = ws.mapit(detectLang(content, it))
    langsMelt = meltSeq langs

  for lm in langsMelt:
    result.add MdNode(kind:  mdsDir,
                      dir:   langs[lm.a], 
                      slice: ws[lm.a].a .. ws[lm.b].b)

proc parseMdSpans*(content: string, slice: Slice[int]): seq[MdNode] = 
  var acc: seq[MdNode]
  var indexes = toDoublyLinkedList([slice])

  for k in [
    # sorted by priority
    mdsWikilink,
    mdsEmbed,
    mdsLink,
    mdsCode,
    mdsMath,
    mdWikiEmbed,
    mdsBoldItalic,
    mdsBold,
    mdsItalic,
    mdsHighlight,
    mdsComment,
    mdsDir,
    mdsText,
  ]:
    proc matchPairInside(l, r: string): Option[Slice[int]] = 
      let r = scrabbleMatchDeepMulti(content, indexes, @[l, r])
      if isSome r:
        let bounds = r.get
        let span = bounds[0].b+1 .. bounds[1].a-1
        result = some span

    while true:
      case k

      of mdsBoldItalic: 
        let v = matchPairInside("***", "***")
        if issome v: acc.add MdNode(kind: k, slice: v.get)
        else: break

      of mdsBold: 
        #  TODO "__" .. "__"
        let v = matchPairInside("**", "**")
        if issome v: acc.add MdNode(kind: k, slice: v.get)
        else: break

      of mdsItalic:
        #  TODO "*" .. "*"
        let v = matchPairInside("_", "_")
        if issome v: acc.add MdNode(kind: k, slice: v.get)
        else: break

      of mdsHighlight:
        # TODO add 🟠 🟢 🟣
        let v = matchPairInside("==", "==")
        if issome v: acc.add MdNode(kind: k, slice: v.get)
        else: break

      of mdsWikilink:
        let r = scrabbleMatchDeepMulti(content, indexes, @["[[", "]]"])
        if isSome r:
          let bounds = r.get
          let area = bounds[0].b+1 .. bounds[1].a-1
          acc.add MdNode(kind: k, slice: area)
          for ni in indexes.nodes:
            if ni.value.intersects area:
              indexes.subtract ni, area
        else:
          break

      of mdsCode:
        let r = scrabbleMatchDeepMulti(content, indexes, @["`", "`"])
        if isSome r:
          let bounds = r.get
          let area = bounds[0].b+1 .. bounds[1].a-1
          acc.add MdNode(kind: k, slice: area)
          for ni in indexes.nodes:
            if ni.value.intersects area:
              indexes.subtract ni, area
        else:
          break

      of mdsMath:
        let r = scrabbleMatchDeepMulti(content, indexes, @["$", "$"])
        if isSome r:
          let bounds = r.get
          let area = bounds[0].b+1 .. bounds[1].a-1
          acc.add MdNode(kind: k, slice: area)
          for ni in indexes.nodes:
            if ni.value.intersects area:
              indexes.subtract ni, area
        else:
          break

      of mdsEmbed:
        let r = scrabbleMatchDeepMulti(content, indexes, @["![", "](", ")"])
        if isSome r:
          let bounds = r.get
          let area1  = bounds[0].b+1 .. bounds[1].a-1
          let area2  = bounds[1].b+1 .. bounds[2].a-1
          let link   = content[area2].strip 

          acc.add MdNode(kind: k, content: link, slice: area1)

          for ni in indexes.nodes:
            for a in [bounds[0], bounds[1], bounds[2], area2]:
              if ni.value.intersects a:
                indexes.subtract ni, a
        else:
          break

      of mdsLink:
        let r = scrabbleMatchDeepMulti(content, indexes, @["[", "](", ")"])
        if isSome r:
          let bounds = r.get
          let area1  = bounds[0].b+1 .. bounds[1].a-1
          let area2  = bounds[1].b+1 .. bounds[2].a-1
          let link   = content[area2].strip 

          acc.add MdNode(kind: k, content: link, slice: area1)

          for ni in indexes.nodes:
            for a in [bounds[0], bounds[1], bounds[2], area2]:
              if ni.value.intersects a:
                indexes.subtract ni, a
        else:
          break

      of mdsComment:
        let head = scrabbleMatchDeep(content, indexes, "// ")
        if issome head:
          let match = scrabbleMatchDeep(content, indexes, "\n")
          let tail = 
            if issome match: match.get.b-1
            else: slice.b
          let span = (head.get.b + 1) .. tail
          acc.add MdNode(kind: k, slice: span)
        else:
          break

      of mdsDir:
        # --- text direction
        var 
          newIndexes: seq[Slice[int]]
        
        for area in indexes:
          let     phrases      = separateLangs(content, area)
          if      phrases.len == 0: continue
          acc.add phrases
         
          # echo phrases.mapit content[it.slice]

          for ph in phrases:
            newIndexes.add ph.slice

        indexes = toDoublyLinkedList newIndexes
        break

      of mdsText:
        # --- remove the escape characters

        for area in indexes: # all other non matched scrabbles
          var cur = area

          # removes escape characters here (splits at escape)
          # support \\ (escaping the escape)
          var again = true
          while again:
            again = false
            for i in cur:
              if content[i] == '\\':
                if content[i+1] == '\\':
                  acc.add MdNode(kind: k, slice: cur.a .. i)
                  cur = i+2 .. cur.b

                else:
                  acc.add MdNode(kind: k, slice: cur.a ..< i)
                  cur = i+1 .. cur.b

                again = true
                break
          

          acc.add MdNode(kind: k, slice: cur)

        break

      else: 
        break


  # aggregate
  proc cmpFirst(a,b: MdNode): int = 
    cmp(a.slice.a, b.slice.a)

  acc.sort cmpFirst

  var root  = MdNode(kind: mdWrap, slice: slice)
  var stack = @[root]

  for node in acc:
    # TODO do not copy
    if node.kind in MdLeafNodes:
      node.content = content[node.slice]

    while true:
      if node.slice in stack[^1].slice:
        stack[^1].children.add node
        
        case node.kind
        of MdLeafNodes: discard
        else:           stack.add node
        break

      else:
        discard stack.pop

  root.children

proc parseMdBlock*(content: string, slice: Slice[int], kind: MdNodeKind): MdNode = 
  let contentslice = onlyContent(content, slice, kind)

  case kind
  
  of mdHLine: 
    MdNode(kind: mdHLine)
  

  of mdbHeader: 
    MdNode(kind: kind,
           priority: skipChar(content, slice, '#') - slice.a,
           children: parseMdSpans(content, contentslice))
  
  of mdbPar: 
    MdNode(kind: kind,
           children: parseMdSpans(content, contentslice))

  of mdbQuote:
    MdNode(kind: kind,
           children: parseMdSpans(content, contentslice))
  

  of mdbMath: 
    MdNode(kind: kind,
           content: content[contentslice])
  
  of mdbCode: 
    let nl = skipAtNextLine(content, contentslice)
    let langslice = contentslice.a .. nl-1
    let codeslice = nl+1 .. contentslice.b-1

    MdNode(kind: kind,
           lang: content[langslice].strip,  # windows uses \r\n for new line :/
           content: content[codeslice].strip)

  of mdWikiEmbed:
    let 
      text = split(content[contentslice], '|')
      url  = text[0].strip
      size = 
        case text.len
        of 1: none int
        else: some text[1].strip.parseInt
    
    MdNode(kind: kind,
           size: size,
           content: url)

  of mdbList:
    var b = MdNode(kind: mdbList)

    # list indicator
    var listId: SimplePattern
    
    for i, id in listPatterns:
      if startsWith(content, slice.a, id) != notfound:
        listId = id
        b.numbered = i == 3
        break

    var acc: seq[Slice[int]]
    var i = slice.a
    var m = notfound

    while i in slice:
      m = startsWith(content, i, listid)
      if m == notfound: raise newException(ValueError, "error list")

      let afternl = find(content, i .. slice.b, "\n")
      let tail = 
        case afternl
        of notfound: slice.b
        else:        afternl

      acc.add m .. tail

      i = tail+1

    for s in acc:
      b.children.add MdNode(kind: mdbPar,
                            children: parseMdSpans(content, s))

    b

  of mdsEmbed:
    TODO 

  else: 
    raise newException(ValueError, fmt"invalid block type '{kind}'")

proc parseMarkdown*(content: string): MdNode = 
  result = MdNode(kind: mdWrap)

  var cursor = 0
  while cursor < content.len:
    let head = skipWhitespaces(content, cursor)
    let kind = detectBlockKind(content, head)
    let tail = afterBlock(content, head, kind)
    if tail - head <= 0: break # maybe that's end of the document
    let b    = parseMdBlock(content, head .. tail-1, kind)
    result.children.add b
    cursor = tail

# ------ Pre-Processors ---------------------------

proc attachNextCommentOfFigAsDesc*(root: sink MdNode): MdNode = 
  ## pipe (preprocessor)
  
  case root.kind
  of mdWrap:

    var newChildren: seq[MdNode]
    var i = 0

    # move comment just after an image to its description
    while i < root.children.len:
      newChildren.add root.children[i]

      # check if the first child of next element which is a par, is a comment
      if i < root.children.len - 1 and 
         root.children[i].kind   == mdWikiEmbed and 
         root.children[i+1].kind == mdbPar and
         root.children[i+1].children[0].kind == mdsComment:
           root.children[i].children.add root.children[i+1].children[0].children
           inc i
      inc i

    root.children = newChildren

  else:
    discard
  
  root

# TODO escape _ in latex
# TODO fix word
# TODO auto link finder (convert normal text -> link) via \url
