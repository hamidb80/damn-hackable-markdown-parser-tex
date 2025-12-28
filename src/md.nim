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
    mdsWikiEmbed # like photos, PDFs, etc ![[...]]

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


proc skipWhitespaces(content: string, cursor: int): int = # : SkipWhitespaceReport = 
  var i = cursor
  while i < content.len:
    if content[i] in Whitespace:
      inc i
    else:
      break
  i

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

proc matches(ch: char, pt: SimplePatternToken): bool = 
  case pt.kind
  of sptChar: ch == pt.ch
  of sptMeta:
    case pt.meta
    of spmWhitespace: ch in Whitespace


proc startsWith(str: string, cursor: int, pattern: SimplePattern): bool = 
  var 
    j = 0      # current pattern
    c = 0      # count
    i = cursor # string index

  while i < str.len:
    if j == pattern.len: # all sub patterns satisfied
      return true
    
    # let cond = matches(str[i], pattern[j].token)
    # echo ">> ", str[i], ' ', pattern[j], ' ', cond

    elif matches(str[i], pattern[j].token):
      inc c
      inc i

      if c >= pattern[j].repeat.b:
        inc j

    elif c in pattern[j].repeat:
        c = 0
        inc j

    else:
      return false


proc detectBlockKind(content: string, slice: Slice[int]): MdNodeKind = 
  # TODO support "^```\s*$" means the line that is started with ``` and ends with ``` and maybe some spaces after
  if   startsWith(content, slice.a, p"```"): mdbCode
  elif startsWith(content, slice.a, p"$$\s" ): mdbMath
  elif startsWith(content, slice.a, p"> "   ): mdbQuote
  elif startsWith(content, slice.a, p"#+ "  ): mdbHeader
  elif startsWith(content, slice.a, p"---+" ): mdHLine
  else: mdbPar

proc skipBefore(content: string, cursor: int, pattern: SimplePattern): int = 
  var i  = cursor
  
  while i < content.len:
    if startsWith(content, i, pattern): return i-1
    else: inc i
  
  raise newException(ValueError, "cannot match end of ")

proc skipChars(content: string, cursor: int, chars: set[char]): int = 
  var i = cursor
  while i < content.len:
    if content[i] notin chars: break
    inc i
  i # or at the end of file

proc skipChar(content: string, cursor: int, ch: char): int = 
  skipChars(content, cursor, {ch})

proc skipNotChar(content: string, cursor: int, ch: char): int = 
  var i = cursor
  while i < content.len:
    if content[i] == ch: break
    inc i
  i # or at the end of file

proc skipAtNextLine(content: string, cursor: int): int = 
  skipNotChar(content, cursor, '\n')

proc skipAfterParagraphSep(content: string, cursor: int): int = 
  # go until double \s+\n\s+\n
  var newlines = 0
  var i = cursor

  while i < content.len:
    case content[i]
    of '\n':                inc newlines
    of Whitespace - {'\n'}: discard
    elif 2 <= newlines:     break
    else:                   reset newlines
    inc i
  
  i
 

proc parseMdBlock(content: string, slice: Slice[int]): Option[MdNode] = 
  # TODO detect indent

  let kind = detectBlockKind(content, slice)
  echo ":: ", kind

  case kind
  of mdbHeader: 
    let e = skipAtNextLine(content, slice.a)
    let i = skipChar(content, slice.a, '#')
    var b = MdNode(kind: mdbHeader, priority: i-slice.a)
    # TODO now go for inline sub nodes
    echo '(' , b.priority, ')', ' ', content[slice.a .. e-1]
  
  of mdHLine: 
    let e = skipAtNextLine(content, slice.a)
    echo "<hr>" , content[slice.a .. e-1]
  
  of mdbPar: 
    let e = skipAfterParagraphSep(content, slice.a)
    echo "(par) ", content[slice.a .. e-1]

    # discard nextSpanCandidate(content, slice.a)
  
  of mdbTable:
     discard
  
  of mdbCode: 
    let pat = "\n```"
    let i = slice.a + len "```"
    let e = skipBefore(content, i, p pat)
    echo "(code) ", content[i..e]
    # e + len pat
  
  of mdbMath: 
    discard
  
  of mdbQuote:
     discard
  
  of mdbList: 
    discard
  
  else: 
    raise newException(ValueError, fmt"invalid block type '{kind}'")


proc parseMarkdown(content: string): MdNode = 
  result = MdNode(kind: mdWrap)

  var cursor = 0
  while true:
    let i  = skipWhitespaces(content, cursor)
    let b  = parseMdBlock(content, i .. content.high)
    # TODO get end position from b
    break
    # cursor = i+1

 
# -----------------------------

when isMainModule:
  echo startsWith("### hello", 0, p"#+\s+")
  echo startsWith("# hello",   0, p"#+\s+")
  echo startsWith("hello",     0, p"#+\s+")
  echo startsWith("```py\n wow\n```", 0, p"```")
# else:
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
