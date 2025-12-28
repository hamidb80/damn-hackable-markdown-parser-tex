import std/[
  strutils, strformat,
  tables,
  options,
  os,
]


type
  MdNodeKind = enum
    # wrapper
    mdWrap

    # metadata
    mdFrontMatter

    # blocks
    mdbHeader
    mdbPar
    mdbTable
    mdbCode
    mdbMath
    mdbQuote
    mdbList

    # spans (inline elements)
    mdsText
    mdsComment
    mdsItalic
    mdsBold
    mdsHighlight
    mdsMath
    mdsCode
    mdsLink
    mdsPhoto
    mdsWikilink
    mdsEmbed # like wiki photos, PDFs, ...

    # other
    mdHLine
  
  MdDir = enum
    unknown
    rtl
    ltr

  MdNode = ref object
    # common
    kind:     MdNodeKind
    children: seq[MdNode]
    content:  string

    # specific
    dir:      MdDir  # for text
    priority: int    # for header
    lang:     string # for code
    href:     string # for link


type
  SkipWhitespaceReport = CountTable[char]

  SimplePatternMeta = enum
    spmWhitespace  # \s

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

# --------------------------------------------

const init = -1

# --------------------------------------------

func xmlRepr(n: MdNode, result: var string) = 
  result.add "<"
  result.add $n.kind
  result.add ">"

  for sub in n.children:
    xmlRepr n, result

  result.add "</"
  result.add $n.kind
  result.add ">"

func xmlRepr(n: MdNode): string = 
  xmlRepr n, result


proc skipWhitespaces(content: string, cursor: int): SkipWhitespaceReport = 
  discard

proc nextSpanCandidate(content: string, cursor: int): int = 
  discard

func at(str: string, index: int): char = 
  if index in str.low .. str.high: str[index]
  else:                            '\0'

proc p(pattern: string): SimplePattern = 
  var i = 0
  while i < pattern.len:
    
    let lastToken = 
      case pattern.at(i)
      of '\\':
        case pattern.at(i+1)
        of 's': 
          inc i
          SimplePatternToken(kind: sptMeta, meta: spmWhitespace)
        else:   
          raise newException(ValueError, fmt"invalid meta character '{pattern.at(i+1)}'")
      else:     
          SimplePatternToken(kind: sptChar, ch: pattern[i])

    let repeat = 
      case pattern.at(i+1)
      of '*':  0 .. int.high
      of '+':  1 .. int.high
      else  :  1 .. 1

    result.add SimplePatternNode(token: lastToken, repeat: repeat)

    if 1 != len repeat:
      inc i, 2
    else:
      inc i 

proc startsWith(str: string, cursor: int, pattern: SimplePattern): bool = 
  false

proc detectBlockKind(content: string, slice: Slice[int]): MdNodeKind = 
  if   startsWith(content, slice.a, p"```\s"):  mdbCode
  elif startsWith(content, slice.a, p"$$\s"):   mdbMath
  elif startsWith(content, slice.a, p">\s"):    mdbQuote
  elif startsWith(content, slice.a, p"---+\s"): mdHLine
  elif startsWith(content, slice.a, p"#+\s"):   mdHLine
  else: mdbPar


proc parseMdBlock(content: string, slice: Slice[int]): Option[MdNode] = 
  # TODO detect indent

  let kind = detectBlockKind(content, slice)

  case kind
  of mdbHeader: 
    discard
  
  of mdbTable:
     discard
  
  of mdbCode: 
    discard
  
  of mdbMath: 
    discard
  
  of mdbQuote:
     discard
  
  of mdbList: 
    discard
  
  of mdHLine: 
    discard
  
  of mdbPar: 
    discard nextSpanCandidate(content, slice.a)
  
  else: 
    raise newException(ValueError, fmt"invalid block type '{kind}'")


proc nextBlockCandidate(content: string, cursor: int): Slice[int] =
  var 
    head = 0 
    tail = content.len - 1

  if cursor != init:
    let r = skipWhitespaces(content, cursor)


  head .. tail

proc parseMarkdown(content: string): MdNode = 
  result = MdNode(kind: mdWrap)

  var cursor = init
  let slice  = nextBlockCandidate(content, cursor)
  let b      = parseMdBlock(content, slice)

 
# -----------------------------

when isMainModule:
  echo p"#+\s+"

  for (t, path) in walkDir "./tests":
    if t == pcFile: 
      let 
        content = readFile path
        doc     = parseMarkdown content
      
      echo "------------- ", path
      echo xmlRepr doc


#[
separate blocks
it's not so easy as a simple RegEx 
since there might be code blocks or math blocks

for each blocks
find span tokens

the overall picture
file (document) 
      consists of block
                  consists of span

any of them can be nested in arbitraty depth
the architecture should be simple and extendable

what about lists and nested lists? are they block or span?
]#
