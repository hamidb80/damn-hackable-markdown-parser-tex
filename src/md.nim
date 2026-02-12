# ----- Imports  ---------------------------------

import std/[
  strformat,
  strutils,
  sequtils, 
  lists, 
  algorithm, 
  tables, 
  options
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
    mdbPar  # paragraph
    mdsLine ## part of paragraph
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
    mdsParen ## []()
    mdsBracket ## []()
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

  # maybe I apply object oriented probramming?
  MdNode* = ref object
    # common
    kind:     MdNodeKind
    children: seq[MdNode]
    content:  string
    slice:    Slice[int]

    # specific
    numbered:  bool   # for list
    dir:       MdDir  # for text
    priority:  int    # for header
    lang:      string # for code
    href:      string # for link

  MdSettings* = object
    langdir*:   MdDir
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

type 
  MdSpan = ref object
    tokens: seq[string]
    maskInside: bool
    kind: MdNodeKind

  PatternNode = ref object
    lookup: Table[char, PatternNode]
    span: MdSpan


# ----- Constants --------------------------------

const notfound* = -1

const MdLeafNodes* = {mdsText, 
                      mdsMath, mdsCode, 
                      mdsEmbed, mdWikiEmbed, mdsWikilink, 
                      mdbMath, mdbCode}

# ----- Syntax Sugar -------------------------------

using 
  content: string
  slice  : Slice[int]
  str    : string
  ch     : char
  chars  : set[char]
  cursor : int
  mask   : var seq[bool]


func mds(tks: seq[string], mi: int, k: MdNodeKind): MdSpan =
  MdSpan(tokens: tks, maskInside: mi == 1, kind: k)
  

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


proc findNotEscapedAfter(content; slice; mask; pattern: string): int = 
  for i in slice:
    let j = i-1
    if content.at(j) != '\\':
      var matched = true
      for k in pattern.indices:
        if at(content, slice, mask, i+k) != pattern[k]:
          matched = false
          break
      
      if matched:
        return min(i + pattern.len, slice.b + 1)

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

func `[]=`(s: var seq[bool], slice: Slice[int], b: bool) = 
  for i in slice:
    s[i] = b
  

proc findFirst(content; slice; mask; patternTree: PatternNode): Option[tuple[cursor: int, pn: MdSpan]] = 
  for i in slice:
    let j = i-1
    if content.at(j) != '\\':
      let m = match(content, i .. slice.b, mask, patternTree)
      if not isnil m:
        return some (i, m)

proc bake(content; slice; mask; spanTokens: seq[MdSpan]): seq[tuple[span: MdSpan, borders: seq[Slice[int]]]] = 
  let pt = makePatternTree(spanTokens)

  while true:
    let qr = findFirst(content, slice, mask, pt)

    if issome qr:
      let 
        span = qr.get.pn
        tks = span.tokens
        i = qr.get.cursor
        subslice = i + tks[0].len .. slice.b
        tail = tks[^1]
        j = 
          if tail == "": # end of line
            subslice.b+1
          else:
            findNotEscapedAfter(content, subslice, mask, tail)

      if j == notfound:
        raise newException(ValueError, fmt"cannot match against {tks}")
      else:
        let 
          b1 = i ..< i+tks[0].len
          b2 = j - tks[^1].len .. j-1
          inside = b1.b+1 .. b2.a-1
          borders = @[b1, b2]
        
        # echo (content[inside], content[b1], content[b2])
        result.add (span, borders)

        # --- mask out
        for b in borders:
          mask[b] = true

        if span.maskInside:
          mask[inside] = true

    else:
      break

# template TODO: untyped =
#   raise newException(ValueError, "TODO")

template `<<`(smth): untyped {.dirty.} =
  result.add smth

# ----- General Utils ------------------------------

# --- char
func isUnicode(ch): bool = 
  127 < ch.uint

# --- seq
func empty(z: seq or string): bool = 
  z.len == 0

func filled(z: seq or string): bool = 
  not empty z

func contains*(n, m: Slice[int]): bool =
  m.a in n and 
  m.b in n


func getWikiLabel*(inner: string): string = 
  ## gets the label from wiki-link or wiki-embed
  ## 
  ## there are 2 possible case:
  ## 1. with    label `[[data science/PCA]]` => PCA
  ## 2. without label `[[data science/PCA | PCA method]]` => PCA method

  let parts = inner.rsplit('|', 1)
  let label = 
    case parts.len
    of 1: parts[0].rsplit('/', 1)[^1]
    else: parts[^1]

  strip label

func getWikiPath*(inner: string): string = 
  inner.split('|', 1)[0].strip

func getWikiEmbedSize*(inner: string): Option[int] = 
  let parts = inner.split('|', 1)
  case parts.len
  of 1: none int
  else: some parts[1].strip.parseInt

# ----- Convertors ---------------------------------

func toXml*(n: MdNode, result: var string) = 
  << "<"
  << $n.kind
  << ">"

  case n.kind
  of MdLeafNodes: << n.content
  else          : discard

  for i, sub in n.children:
    if i != 0: << ' '
    toXml sub, result

  << "</"
  << $n.kind
  << ">"

func toXml*(n: MdNode): string = 
  toXml n, result


func writeEscapedTex*(content; result: var string) = 
  for ch in content:
    if ch in {'\\', '_', '^', '%'}:
      << '\\'
    << ch

func toTex*(n: MdNode, settings: MdSettings, result: var string) = 
  case n.kind

  of mdWrap:
    for i, sub in n.children:
      toTex sub, settings, result
      << "\n\n"
  
  of mdbHeader:
    let tag = 
      case n.priority
      of 1: "section"
      of 2: "subsection"
      of 3: "subsubsection"
      else: "par"

    << '\\'
    << tag
    << '{'
    for i, sub in n.children:
      if i != 0: << ' '
      toTex sub, settings, result
    << '}'

  of mdbPar:
    for i, sub in n.children:
      if i != 0: << '\n' # the children of paragraph are lines
      toTex sub, settings, result

  of mdsLine:
    for i, sub in n.children:
      if i != 0: << ' '
      toTex sub, settings, result

  of mdsDir: 
    # \usepackage{bidi}
    # \lr : ltr 
    # \rl : rtl

    let tag =
      if settings.langdir == mddUndecided: ""
      elif n.dir != settings.langdir: 
        case n.dir
        of   mddLtr:       "lr"
        of   mddRtl:       "rl"
        of   mddUndecided: ""
      else:  ""

    if filled tag:
      << '\\'
      << tag
      << '{'

    for i, sub in n.children:
      toTex sub, settings, result
  
    if filled tag:
      << '}'
 
  of mdsCode: 
    << "\\texttt{"
    << n.content
    << '}'

  of mdsMath: 
    << "\\("
    << n.content
    << "\\)"

  of mdbMath: 
    << "\\[\n"
    << n.content
    << "\n\\]"

  of mdbCode:
    << "\\begin{verbatim}\n"
    << n.content
    << "\n\\end{verbatim}"

  of mdsBoldItalic: 
    << "\\verb{"
    for i, sub in n.children:
      if i != 0: << ' '
      toTex sub, settings, result
    << "}"

  of mdsBold: 
    << "\\textbf{"
    for i, sub in n.children:
      if i != 0: << ' '
      toTex sub, settings, result
    << "}"

  of mdsItalic: 
    << "\\textit{"
    for i, sub in n.children:
      if i != 0: << ' '
      toTex sub, settings, result
    << "}"

  of mdsHighlight: 
    << "\\hl{"
    for i, sub in n.children:
      if i != 0: << ' '
      toTex sub, settings, result
    << "}"

  of mdsComment:
    << "\\begin{small}"
    for i, sub in n.children:
      if i != 0: << ' '
      toTex sub, settings, result
    << "\\end{small}"

  of mdsText: 
    writeEscapedTex n.content, result

  of mdHLine: 
    << "\\clearpage"

  of mdsWikilink:
    toTex MdNode(kind: mdsItalic, children: @[
      MdNode(kind: mdbPar, children: @[
        MdNode(kind: mdsText, content: getWikiLabel n.content)
      ])]), settings, result

  of mdWikiEmbed:
    << "\\begin{figure}[H]\n"
    << "\\centering\n"
    << "\\includegraphics["
    let s = n.content.getWikiEmbedSize
    if isSome s: 
      let size = (s.get / settings.pageWidth) * (15)
      << "width="
      << formatFloat(size, precision=3)
      << "cm,"
    << "keepaspectratio]{"
    << n.content
    << "}\n"
    << "\\caption{"
    for i, sub in n.children:
      if i != 0: << ' '
      toTex sub, settings, result
    << "}\n"
    << "\\end{figure}"

  # of mdsEmbed:
  #   TODO

  of mdFrontMatter:
    discard

  of mdbList:
    let tag = 
      if n.numbered: "enumerate"
      else:          "itemize"
    << "\\begin{"
    << tag
    << "}"
    for i, sub in n.children:
      << "\n\\item "
      toTex sub, settings, result
    << "\n\\end{"
    << tag
    << "}"

  of mdsLink:
    << "\\href{"
    << n.content
    << "}"

    if n.children.len > 0:
      << "{"
      for i, sub in n.children:
        if i != 0: << ' '
        toTex sub, settings, result
      << "}"

  of mdsParen:
    << "("
    for i, sub in n.children:
      if i != 0: << ' '
      toTex sub, settings, result
    << ")"

  of mdsBracket:
    << "["
    for i, sub in n.children:
      if i != 0: << ' '
      toTex sub, settings, result
    << "]"

  of mdbQuote, mdbTable, mdsEmbed:
    raise newException(ValueError, fmt"toTex for kind {n.kind} is not implemented")

func toTex*(n: MdNode, settings: MdSettings): string = 
  toTex n, settings, result

# ----- Matching Utils -----------------------------

func p*(str): SimplePattern = 
  ## make pattern

  var i = 0
  while i < str.len:
    
    let lastToken = 
      case str.at(i)
      of '\\':
        inc i
        case str.at(i)
        of 's':        SimplePatternToken(kind: sptMeta, meta: spmWhitespace)
        of 'd':        SimplePatternToken(kind: sptMeta, meta: spmDigit)
        of '\\', '+':  SimplePatternToken(kind: sptChar, ch: str[i])
        else:    raise newException(ValueError, fmt"invalid meta character '{str.at(i+1)}'")
      else:     
          SimplePatternToken(kind: sptChar, ch: str[i])

    let repeat = 
      case str.at(i+1)
      # of '^':  0 .. 0
      # of '*':  0 .. int.high
      of '+':  1 .. int.high
      else  :  1 .. 1

    result.add SimplePatternNode(token: lastToken, repeat: repeat)

    if 1 != len repeat:
      inc i, 2
    else:
      inc i 

const listPatterns = [p"- ", p"\+ ", p"* ", p"\d+. "]

func matches*(ch; pt: SimplePatternToken): bool = 
  case pt.kind
  of sptChar: ch == pt.ch
  of sptMeta:
    case pt.meta
    of spmWhitespace: ch in Whitespace
    of spmDigit     : ch in Digits

func find*(content; slice; str): int = 
  var i = slice.a
  var j = 0 
  
  while i in slice:
    if content[i] == str[j]:
      inc j
      if j == str.len: return i-j+1
    else:
      dec i, j
      reset j

    inc i

  notfound

func skipWhitespaces*(content; cursor): int =
  var i = cursor
  while i < content.len:
    if content[i] in Whitespace:
      inc i
    else:
      break
  i

func startsWith*(str; cursor; pattern: SimplePattern): int = 
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

func skipBefore*(content; cursor; pattern: SimplePattern): int = 
  var i  = cursor
  
  while i < content.len:
    if startsWith(content, i, pattern) != notfound: return i-1
    inc i
  
  raise newException(ValueError, "cannot match end of " & $pattern)

func stripSlice*(content; slice; chars): Slice[int] = 
  var i = slice.a
  var j = slice.b

  while content[i] in chars: inc i
  while content[j] in chars: dec j
  
  i .. j

func skipChars*(content; slice; chars): int = 
  var i = slice.a
  while i in slice:
    if content[i] notin chars: break
    inc i
  i # or at the end of file

func skipChar*(content; slice; ch): int = 
  skipChars(content, slice, {ch})

func skipNotChar*(content; slice; ch): int = 
  var i = slice.a
  while i in slice:
    if content[i] == ch: break
    inc i
  i # or at the end of file

func skipAtNextLine*(content; slice): int = 
  skipNotChar(content, slice, '\n')

# ----- Main Functionalities ---------------------

func detectBlockKind*(content; cursor): MdNodeKind = 
  if   startsWith(content, cursor, p"```")   != notfound: mdbCode
  elif startsWith(content, cursor, p"$$\s")  != notfound: mdbMath
  elif startsWith(content, cursor, p"> ")    != notfound: mdbQuote
  elif startsWith(content, cursor, p"#+ ")   != notfound: mdbHeader
  elif startsWith(content, cursor, p"---+")  != notfound: mdHLine
  elif startsWith(content, cursor, p"![[")   != notfound: mdWikiEmbed
  elif startsWith(content, cursor, p"![")    != notfound: mdsEmbed
  elif listPatterns.anyit(notfound != startsWith(content, cursor, it)): mdbList
  else: mdbPar

func skipAfterParagraphSep*(content; slice; kind: MdNodeKind): int = 
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

func afterBlock*(content; cursor; kind: MdNodeKind): int = 
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

func onlyContent*(content; slice; kind: MdNodeKind): Slice[int] = 
  case kind
  of mdbCode:      stripSlice(content, slice, {'`'}                       )
  of mdbMath:      stripSlice(content, slice, {'$'}           + Whitespace)
  of mdbHeader:    stripSlice(content, slice, {'#'}           + Whitespace)
  of mdbQuote:     stripSlice(content, slice, {'>'}           + Whitespace)
  of mdbPar:       stripSlice(content, slice,                   Whitespace)
  of mdWikiEmbed:  stripSlice(content, slice, {'!', '[', ']'} + Whitespace)
  else: slice

func detectLang(content; slice): MdDir =
  for i in slice:
    if isUnicode    content[i]: return mddRtl
    if isAlphaAscii content[i]: return mddLtr
  return mddUndecided

func wordSlices(content; slice): seq[Slice[int]] =
  var changes: seq[int]
  var l = true # last was whitespace?

  for i in slice:
    let w = content[i] in Whitespace
    if  w != l:
      changes.add i
    l = w

  if changes.filled and not l:
    changes.add slice.b+1

  for i in countup(1, changes.high, 2):
    let head = changes[i-1]
    let tail = changes[i]-1
    result.add head..tail

func meltSeq(elements: seq[MdDir]): seq[Slice[int]] = 
  var j = 0

  for i in 1 ..< elements.len:
    if elements[i] != mddUndecided and 
       elements[i] != elements[j]:
      result.add j ..< i
      j = i

  let s = j ..< elements.len
  if 1 <= len s:
    result.add s

func separateLangs*(content; slice): seq[MdNode] =
  let 
    ws        = wordSlices(content, slice)
    langs     = ws.mapit(detectLang(content, it))
    langsMelt = meltSeq langs

  for lm in langsMelt:
    result.add MdNode(kind:  mdsDir,
                      dir:   langs[lm.a], 
                      slice: ws[lm.a].a .. ws[lm.b].b)

func freeSlices(mask; slice;): seq[Slice[int]] = 
  var pin  = notfound

  for i in slice.a .. slice.b+1:
    if i notin slice or mask[i]:
      if pin == notfound: discard
      else:
        result.add pin .. i-1
        pin = notfound

    elif pin == notfound:
      pin = i

proc linesSlice(content; slice;): seq[Slice[int]] = 
  # for each line
  var i = slice.a
  while i < slice.b:
    let s = skipAtNextLine(content, i .. slice.b)
    result.add i .. s-1 
    i = s+1


proc parseParMdSpans*(content; slice; mask): seq[MdNode] = 
  var acc: seq[MdNode]

  for ls in linesSlice(content, slice):
    acc.add MdNode(kind: mdsLine, slice: ls)

    for p in [
      @[
        mds(@["`"], 1, mdsCode), 
        mds(@["$"], 1, mdsMath),
        mds(@["[[", "]]"], 1, mdsWikilink),
      ],

      @[
        mds(@["(", ")"],0,mdsParen),
        mds(@["[", "]"],0,mdsBracket),
      ],

      @[
        mds(@["**"],0,mdsBold), 
        mds(@["=="],0,mdsHighlight),
      ],

      @[
        mds(@["_"],0, mdsItalic),
      ],

      @[
        mds(@["// ", ""], 0, mdsComment), # means EOL i.e. end of line
      ],
    ]:
      let matches = bake(content, ls, mask, p)
      var i = 0
      while i < matches.len:
        var k = matches[i].span.kind
        var add = true

        case k
        # of mdsWikilink:
        #   if at(content, slice, mask, matches[i].borders[0].a-1) == '!': 
        #     k = mdWikiEmbed

        # TODO add embed

        of mdsBracket:
          let j = i+1
          if j in matches.indices and 
            matches[j].span.kind == mdsParen and 
            matches[i].borders[^1].b+1 == matches[j].borders[0].a
          :
            let linkSlice = matches[j].borders[0].a+1 .. matches[j].borders[^1].b-1
            let inside    = matches[i].borders[0].a+1 .. matches[j].borders[^1].b-1
            acc.add MdNode(
              kind: mdsLink,
              slice: inside,
              content: content[linkSlice],
              )

            mask[linkSlice] = true # mask inside
            add = false
            inc i

        else:
          discard

        if add:
          acc.add MdNode(kind: k, 
                        slice: matches[i].borders[0].b+1 .. matches[i].borders[^1].a-1)
        
        inc i

  var 
    indexes: DoublyLinkedList[Slice[int]]
    
  # dir and text ...
  block dir_detect:
    # --- text direction
    var 
      newIndexes: seq[Slice[int]]
    
    for area in freeSlices(mask, slice):
      let     phrases      = separateLangs(content, area)
      if      phrases.len == 0: continue
      acc.add phrases
      
      for ph in phrases:
        newIndexes.add ph.slice

    indexes = toDoublyLinkedList newIndexes

  block text_sep:
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
            if content.at(i+1) == '\\':
              acc.add MdNode(kind: mdsText, slice: cur.a .. i)
              cur = i+2 .. cur.b

            else:
              acc.add MdNode(kind: mdsText, slice: cur.a ..< i)
              cur = i+1 .. cur.b

            again = true
            break
      
      acc.add MdNode(kind: mdsText, slice: cur)


  # aggregate
  func cmpFirst(a,b: MdNode): int = 
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

proc parseMdBlock*(content; slice; mask; kind: MdNodeKind): MdNode = 
  let contentslice = onlyContent(content, slice, kind)

  case kind
  
  of mdHLine: 
    MdNode(kind: mdHLine)
  

  of mdbHeader: 
    MdNode(kind: kind,
           priority: skipChar(content, slice, '#') - slice.a,
           children: parseParMdSpans(content, contentslice, mask))
  
  of mdbPar: 
    MdNode(kind: kind,
           children: parseParMdSpans(content, contentslice, mask))

  of mdbQuote:
    MdNode(kind: kind,
           children: parseParMdSpans(content, contentslice, mask))
  

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
    MdNode(kind: kind,
           content: content[contentslice])

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
                            children: parseParMdSpans(content, s, mask))

    b

  # of mdsEmbed:
  else: 
    raise newException(ValueError, fmt"invalid block type '{kind}'")

proc parseMarkdown*(content): MdNode = 
  result = MdNode(kind: mdWrap)
  var mask = newSeqWith(content.len, false)

  var cursor = 0
  while cursor < content.len:
    let head = skipWhitespaces(content, cursor)
    let kind = detectBlockKind(content, head)
    let tail = afterBlock(content, head, kind)
    if tail - head <= 0: break # maybe that's end of the document
    let b    = parseMdBlock(content, head .. tail-1, mask, kind)
    result.children.add b
    cursor = tail

# ------ Pre-Processors ---------------------------

func attachNextCommentOfFigAsDesc*(root: sink MdNode): MdNode = 
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
         root.children[i+1].children[0].children[0].kind == mdsComment:
           root.children[i].children.add root.children[i+1].children[0].children
           inc i
      inc i

    root.children = newChildren

  else:
    discard
  
  root

# TODO auto link finder (convert normal text -> link) via \url
# TODO add table parser
# TODO add footnote
# TODO support enumerated list in form of a. b. c. ...
