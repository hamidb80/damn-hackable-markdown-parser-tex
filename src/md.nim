# ----- Imports  ---------------------------------

import std/[
  strformat,
  strutils,
  sequtils, 
  lists, 
  algorithm, 
  tables, 
  options,
  json,
  nre,
]

# import pretty

# ----- Type Defs  -------------------------------

type
  MdNodeKind* = enum
    # wrapper
    mdWrap

    # metadata
    mdComment
    mdChapter
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
    mdsFootNote ## (( ... ))
    mdsCite ## {{ ... }}
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
    kind*:     MdNodeKind
    children*: seq[MdNode]
    content*:  string
    slice*:    Slice[int]

    # specific
    numbered*:  bool   # for list
    dir*:       MdDir  # for text
    priority*:  int    # for header
    lang*:      string # for code
    href*:      string # for link

    spaceBefore*: bool

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

func `+`[T](i: T; s: Slice[T]): Slice[T] = 
  s.a + i .. s.b + i

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

template `<<`(smth): untyped {.dirty.} =
  result.add smth

# ----- General Utils ------------------------------

# --- char
func isUnicode(ch): bool = 
  127 < ch.uint

# func isAscii(ch): bool = 
#   not isUnicode ch

# --- seq
func isEmpty(z: seq or string): bool = 
  z.len == 0

func filled(z: seq or string): bool = 
  not isEmpty z

func validSlice(slice): bool =
  slice.a <= slice.b

func strip(s:  string, slice: Slice[int], chars: set[char] = Whitespace): Slice[int] = 
  if validSlice slice:
    var 
      i = 0
      j = 0

    while s[slice.a + i] in chars and slice.a + i < slice.b + j: inc i
    while s[slice.b + j] in chars and slice.a + i < slice.b + j: dec j

    slice.a + i .. slice.b + j

  else:
    slice

func contains*(n, m: Slice[int]): bool =
  m.a in n and 
  m.b in n

# --- wiki-link
func getWikiLabelSlice*(inner: string): Slice[int] = 
  ## gets the label from wiki-link or wiki-embed
  ## 
  ## there are 2 possible case:
  ## 1. with    label `[[data science/PCA]]` => PCA
  ## 2. without label `[[data science/PCA | PCA method]]` => PCA method

  let sep = inner.rfind('|')
  let rng = 
    case sep
    of -1: inner.rfind('/')+1 .. inner.high
    else : sep+1 .. inner.high
  
  strip inner, rng

func getWikiPathSlice*(inner: string): Slice[int] = 
  let sep = inner.rfind('|')
  let rng = 
    case sep
    of -1: 0 .. inner.high
    else : 0 .. sep-1

  strip inner, rng


func getWikiEmbedSize*(inner: string, fallback: int = 0): int = 
  let parts = inner.split('|', 1)
  case parts.len
  of 1: fallback
  else: parts[1].strip.parseInt

# ----- Convertors ---------------------------------

func prettyName(k: MdNodeKind): string = 
  case k
  of mdWrap: "wrapper"
  of mdComment: "comment"
  of mdChapter: "chapter"
  of mdFrontMatter: "front-matter"
  of mdbHeader: "header"
  of mdbPar: "paragraph"
  of mdsLine: "line"
  of mdbCode: "code-block"
  of mdbMath: "math-block"
  of mdbQuote: "quote"
  of mdbList: "list"
  of mdbTable: "table"
  of mdsDir: "text-direction"
  of mdsBoldItalic: "bold-italic"
  of mdsBold: "bold"
  of mdsItalic: "italic"
  of mdsHighlight: "highlight"
  of mdsCode: "inline-code"
  of mdsMath: "inline-math"
  of mdsLink: "link"
  of mdsParen: "paren"
  of mdsBracket: "square-bracket"
  of mdsEmbed: "inline-embed"
  of mdsWikilink: "wiki-link"
  of mdsFootNote: "footnote"
  of mdsCite: "cite"
  of mdsComment: "comment"
  of mdWikiEmbed: "wiki-embed"
  of mdsText: "text"
  of mdHLine: "horizontal-line"

func prettyName(d: MdDir): string = 
  case d
  of mddLtr: "lrt"
  of mddRtl: "rtl"
  of mddUndecided: "undecided"


func addJsonKey*(result: var string, key: string) = 
  << '"'
  << key
  << '"'
  << ':'

template adjk(smth; sep=true): untyped {.dirty.} =
  if sep: result.add ','
  addJsonKey result, smth

template adjv(str: string): untyped {.dirty.} =
  escapeJson str, result

template adjv(num: int): untyped {.dirty.} =
  add result, $num

template adjv(cond: bool): untyped {.dirty.} =
  add result, $cond

template adjp(key, val): untyped {.dirty.} =
  adjk key
  adjv val


func toJson*(n: MdNode, result: var string) = 
  # raise newException(ValueError, "TODO")

  << '{'
  adjk "kind", false
  adjv prettyName n.kind

  # --- content
  case n.kind
  of mdWrap, mdChapter, mdbPar, mdsLine, mdbQuote, mdsBoldItalic, mdsBold, mdsItalic, mdsParen, mdsBracket, mdsComment, mdsCite, mdsFootNote, mdHLine, mdsHighlight: 
    discard

  of mdComment:
    adjp "comment", n.content
  
  of mdbHeader: 
    adjp "level", n.priority
  
  of mdbCode, mdbMath: 
    adjp "content", n.content
  
  of mdsDir: 
    adjp "dir", prettyName n.dir

  of mdbList: 
    adjp "numbered", n.numbered
  
  of mdsCode, mdsMath, mdsText: 
    adjp "content", n.content
  
  of mdsLink: 
    adjp "ref", n.content
  
  of mdsEmbed: 
    adjp "ref", n.content[getWikiPathSlice n.content]
  
  of mdsWikilink: 
    adjp "ref", n.content[getWikiPathSlice n.content]
  
  of mdWikiEmbed: 
    adjp "ref", n.content[getWikiPathSlice n.content]
    adjp "size", getWikiEmbedSize n.content

  of mdFrontMatter: 
    adjp "content", n.content # TODO convert to json

  of mdbTable:
    raise newException(ValueError, "TODO")

  # --- children

  case n.kind
  of MdLeafNodes: discard
  else:
    adjk "children"
    << '['

    for i, sub in n.children:
      if 0 < i: << ','
      toJson sub, result      

    << ']'
  result.add "}"

func toJson*(n: MdNode): string = 
  toJson n, result


func writeEscapedTex*(content; result: var string) =
    # of '<': ("\\textless ")
    # of '>': ("\\textgreater ")
    # of '[': ("{[}")
    # of ']': ("{]}")

  for ch in content:
    case ch
    of '\\', '_', '^', '%', '$', '#', '{', '}', '&':
      << '\\'
      << ch
    else:
      << ch

func toTex*(n: MdNode, settings: MdSettings, result: var string) = 
  if n.spaceBefore: << ' '

  case n.kind

  of mdWrap:
    for i, sub in n.children:
      toTex sub, settings, result
      << "\n\n"

  of mdComment:
      << '%'
      << ' '
      << n.content
      << '\n'

  of mdChapter:
    << "\\chapter{"
    for i, sub in n.children:
      toTex sub, settings, result
    << "}"
    << "\n"
    << "\\clearpage"
    << "\n"
  
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
    << "\\textbf{"
    for i, sub in n.children:
      if i != 0: << ' '
      toTex sub, settings, result
    << '}'
    << '}'

  of mdbPar:
    for i, sub in n.children:
      if i != 0: << '\n' # the children of paragraph are lines
      toTex sub, settings, result

  of mdsLine:
    for i, sub in n.children:
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
    let s = settings.langdir == mddRtl

    if s: << "\\ltr{"
    << "\\texttt{"
    writeEscapedTex n.content, result
    << '}'
    if s: << "}"

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

  of mdsCite:
    << "cite"
    << n.content
    << "}"

  of mdsFootNote:
    << "\\footnote{"
    for i, sub in n.children:
      if i != 0: << ' '
      toTex sub, settings, result
    << "}"

  of mdsText: 
    writeEscapedTex n.content, result

  of mdHLine: 
    << "\\clearpage"

  of mdsWikilink:
    for i, sub in n.children:
      toTex sub, settings, result
    << " "
    << "\\cite{"
    << n.content[n.content.getWikiPathSlice]
    << "}"

  of mdWikiEmbed:
    << "\\begin{figure}[H]\n"
    << "\\centering\n"
    << "\\includegraphics["
    let s = n.content.getWikiEmbedSize
    if s != 0: 
      let size = (s / settings.pageWidth) * (15)
      << "width="
      << formatFloat(size, precision=3)
      << "cm,"
    << "keepaspectratio]{"
    << n.content[getWikiPathSlice n.content]
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
    for l in n.content.splitLines:
      << '%'
      << ' '
      << l
      << '\n'

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

  of mdbQuote:
    for i, sub in n.children:
      if i != 0: << ' '
      toTex sub, settings, result

  of mdbTable, mdsEmbed:
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

const listPatterns = [p"- ", p"\+ ", p"* ", p"\d+. ", p"\d+) "]

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


func initMaskOf*(content): seq[bool] = 
  newSeqWith content.len, false

proc markSpace(root: MdNode; mask; content) = 
  var stack = @[@[root]]
  var last_leaf: MdNode = nil

  while stack.filled:
    let hie = stack.pop
    let cur = hie[^1]

    if cur.kind in MdLeafNodes + {mdsParen, mdsBracket}:

      if not last_leaf.isNil:
        let rng = last_leaf.slice.b+1 .. cur.slice.a-1
        for j in rng:
          if content[j] in {' ', '\t'}:
            # propagate space to parent, tries to push the leading space out of block

            for i in 1 .. hie.len-1:
              let node = hie[^i]
              let parent = hie[^(i+1)]
              let isFirst = parent.children[0] == node

              if not isFirst:
                node.spaceBefore = true 
                break

            break

      last_leaf = cur
    
    for i in 1 .. cur.children.len:
      stack.add hie & cur.children[^i]

proc parseParMdSpans*(content; slice; mask): seq[MdNode] = 
  var acc: seq[MdNode]

  for ls in linesSlice(content, slice):
    acc.add MdNode(kind: mdsLine, slice: ls)

    for p in [
      @[
        mds(@["`"], 1, mdsCode), 
        mds(@["$"], 1, mdsMath),
        mds(@["[[", "]]"], 1, mdsWikilink), 
        mds(@["((", "))"], 0, mdsFootNote), 
        mds(@["{{", "}}"], 1, mdsCite), 
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
        var sliceContent = ""

        case k
        of mdsWikilink:
          # if at(content, slice, mask, matches[i].borders[0].a-1) == '!': 
          #   k = mdWikiEmbed
          let 
            m            = matches[i]
            contentSlice = m.borders[0].b+1 .. m.borders[1].a-1
            labelSlice   = contentSlice.a + getWikiLabelSlice(content[contentSlice])
          
          sliceContent = content[contentSlice]
          for c in contentSlice:
            mask[c] = c notin labelSlice

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
                      content: sliceContent,
                        slice: matches[i].borders[0].b+1 .. matches[i].borders[^1].a-1)
        
        inc i

  var 
    indexes: DoublyLinkedList[Slice[int]]
    
  # dir and text ...
  block dir_detect: # --- text direction
    var 
      newIndexes: seq[Slice[int]]
    
    for area in freeSlices(mask, slice):
      let     phrases      = separateLangs(content, area)
      if      phrases.len == 0: continue
      acc.add phrases
      
      for ph in phrases:
        newIndexes.add ph.slice

    indexes = toDoublyLinkedList newIndexes

  block text_sep: # --- remove the escape characters
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

  markSpace root, mask, content

  root.children

proc parseParMdSpans*(content): seq[MdNode] = 
  var mask  = newSeqWith(content.len, false)
  var slice = content.low .. content.high
  parseParMdSpans(content, slice, mask)

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
        b.numbered = i >= 3
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

func frontMatterIndex(content; slice): Slice[int] = 
  if content[slice.a + (0 ..< 3)] == "---" and content[slice.a + 3] != '-':
    let 
      i1 = find(content, "\n---"  , slice.a + 3) 
      i2 = find(content, "\n\r---", slice.a + 3)
      tail = 
        if i1 != -1: i1
        else:        i2
      lenn = 
        if i1 != -1: 4
        else:        5

    if tail != -1:
      return 0 .. tail + lenn

  0 .. -1

proc parseMarkdown*(content; result: MdNode) = 
  var mask   = initMaskOf content
  let fslice = frontMatterIndex(content, indices content)

  if 0 < len fslice:
    mask[fslice] = true
    result.children.add MdNode(kind: mdFrontMatter, 
                              slice: fslice, 
                              content: strip(content[fslice], chars={'-', '\n', '\r', ' '}))

  var cursor = fslice.b + 1
  while cursor < content.len:
    let head = skipWhitespaces(content, cursor)
    let kind = detectBlockKind(content, head)
    let tail = afterBlock(content, head, kind)
    if  tail - head <= 0: break # end of the document
    let b    = parseMdBlock(content, head .. tail-1, mask, kind)
    result.children.add b
    cursor = tail

proc parseMarkdown*(content): MdNode = 
  result = MdNode(kind: mdWrap)
  parseMarkdown content, result

# ------ Pre-Processors ---------------------------

const semiSpace = "\u200c" # persian semi space

func fixCommonPersianTypos*(s: string): string =
  multiReplace(s, [
    # spell checks
    ("جزییات", "جزئیات"),
    ("هییت", "هیئت"),
    ("تاثیر", "تأثیر"),
    ("سوال", "سؤال"),
    ("مؤسس", "موسس"),
    
    # arabic correctness
    ("دقیقا", "دقیقاً"),
    # ("اصلا", "اصلاً"), # mistaken with اصلاحی
    ("مثلا", "مثلاً"),
    ("اولا", "ًاولا"),
    ("ثانیا", "ثانیاً"),
    # ("اتفاقا", "ًاتفاقا"), # XXX can be mistaken with اتفاقات

    # style preference
    ("آنها", "آن ها"),
    ("آنقدر", "آن\u200cقدر"),
    ("بیش تر", "بیش\u200cتر"),
    ("بیشتر", "بیش\u200cتر"),
    ("کمتر", "کم\u200cتر"),
    ("کم تر", "کم\u200cتر"),
    ("پایینتر", "پایین\u200c"),
    ("پایین تر", "پایین\u200c"),
    
    # continous verbs 
    ("میشو", "می شو"),
    ("میشد", "می شد"),
    ("میکن", "می کن"),
    ("میکرد", "می کرد"),
    ("میتوا", "می توا"),
    ("میبای", "می بای"),
    ("میگی", "می گی"),
    ("میرسد", "می رسد"),
    ("میرسند", "می رسند"),
    ("میرود", "می رود"),
    ("میروند", "می روند"),
    ("میگویند", "می گویند"),
    ("میگوید", "می گوید"),
    ("میدهد", "می دهد"),
    ("میدهند", "می دهند"),
    ("میداد", "می داد"),
    
    ("شده است", "شده\u200cاست"),
    ("بوده است", "بوده\u200cاست"),
    ("داده است", "داده\u200cاست"),
    ("شده اند", "شده\u200cاند"),
    ("بوده اند", "بوده\u200cاند"),
    ("داده اند", "داده\u200cاند"),

  ]).replace(" " & "ها", semiSpace & "ها") # XXX make your you don't write  هان هایده هایما هار هال هاب ...

proc removePersianSpace*(s: string): string =
  func repl(m: RegexMatch): string = 
    if m.captureBounds[0].a == 0 or not s[m.captureBounds[0].a-1].isUnicode:
      m.captures[0] & semiSpace & m.captures[1]
    else:
      # debugecho m.str
      # debugEcho (s[m.captureBounds[0].a-1].uint8)
      # debugEcho "'", (m.str.substr(m.captureBounds[0].a-2)), "'"
      m.match

  result = s
  result = replace(result, re("(نمی|می)" & " " & "([ا-ی])"), repl) # ستون ها
  result = replace(result, re("(هم)"    & " " & "([ا-ی])"), repl) # هم تراز

  
proc persianContVerbFixerImpl(n: MdNode) =
  case n.kind
  of mdsText:
    n.content = removePersianSpace fixCommonPersianTypos n.content

  else:
    for ch in n.children:
      persianContVerbFixerImpl ch

proc persianContVerbFixer*(root: sink MdNode): MdNode =
  # می فعل -> می،فعل
  persianContVerbFixerImpl root
  root


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
# TODO support enumerated list in form of a. b. c. ...
# FIXME support for link inside parens ([label](url))
# FIXME the label of wikilink does not get direction detection
