import std/[
  strutils, strformat,
  tables,
  options,
  os,
]


type
  MdNodeKind = enum
    # wrapper
    mdWrap # XXX add indent to this

    # metadata
    mdFrontMatter # yaml

    # blocks
    mdbHeader # ###
    mdbPar 
    mdbTable
    mdbCode # ``` 
    mdbMath # $$
    mdbQuote # > 
    mdbList # + - *

    # spans (inline elements)
    mdsText # ...
    mdsComment # // ...
    mdsItalic # *...* _..._
    mdsBold # **...**
    mdsHighlight # ==...==
    mdsMath # $...$
    mdsCode # `...`
    mdsLink # ![]()
    mdsEmbed # ![]()
    mdsWikilink # [[ ... ]]
    mdsWikiEmbed # like photos, PDFs, etc ![[...]]

    # other
    mdHLine # ---
  
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


proc skipWhitespaces(content: string, cursor: int): int = # : SkipWhitespaceReport = 
  var i = cursor
  while i < content.len:
    if content[i] in Whitespace:
      inc i
    else:
      break
  i

proc startsWith(str: string, cursor: int, pattern: SimplePattern): bool = 
  var 
    j = 0      # current pattern
    c = 0      # count
    i = cursor # string index

  while true:
    if i >= str.len: 
      return false
    
    if j == pattern.len: # all sub patterns satisfied
      return true
    
    # let cond = matches(str[i], pattern[j].token)
    # echo ">> ", str[i], ' ', pattern[j], ' ', cond

    if matches(str[i], pattern[j].token):
      inc c
      inc i

      if c >= pattern[j].repeat.b:
        inc j

    elif c in pattern[j].repeat:
        c = 0
        inc j

    else:
      return false
  
  true

proc skipBefore(content: string, cursor: int, pattern: SimplePattern): int = 
  var i  = cursor
  
  while i < content.len:
    if startsWith(content, i, pattern): return i-1
    else: inc i
  
  raise newException(ValueError, "cannot match end of " & $pattern)

proc stripSlice(content: string, slice: Slice[int], chars: set[char]): Slice[int] = 
  var i = slice.a
  var j = slice.b

  while content[i] in chars: inc i
  while content[j] in chars: dec j
  
  i .. j

proc skipChars(content: string, slice: Slice[int], chars: set[char]): int = 
  var i = slice.a
  while i in slice:
    if content[i] notin chars: break
    inc i
  i # or at the end of file

proc skipChar(content: string, slice: Slice[int], ch: char): int = 
  skipChars(content, slice, {ch})

proc skipNotChar(content: string, slice: Slice[int], ch: char): int = 
  var i = slice.a
  while i in slice:
    if content[i] == ch: break
    inc i
  i # or at the end of file

proc skipAtNextLine(content: string, slice: Slice[int]): int = 
  skipNotChar(content, slice, '\n')


proc detectBlockKind(content: string, cursor: int): MdNodeKind = 
  if   startsWith(content, cursor, p"```"):   mdbCode
  elif startsWith(content, cursor, p"$$\s" ): mdbMath
  elif startsWith(content, cursor, p"> "   ): mdbQuote
  elif startsWith(content, cursor, p"#+ "  ): mdbHeader
  elif startsWith(content, cursor, p"---+" ): mdHLine
  # TODO add list and embed
  else: mdbPar

proc skipAfterParagraphSep(content: string, slice: Slice[int]): int = 
  ## go until double \s+\n\s+\n

  var newlines = 0
  var i = slice.a

  while i in slice:
    case content[i]
    of '\n':                
      inc newlines
      if detectBlockKind(content, i+1) != mdbPar: break # there is one exception and that is if there be a list just after the paragraph or $$ or ``` or ![[ or ---- :(

    of Whitespace - {'\n'}: discard
    elif 2 <= newlines:     break
    else:                   reset newlines
    inc i
  
  i

proc afterBlock(content: string, cursor: int, kind: MdNodeKind): int = 
  case kind
  of mdbHeader: skipAtNextLine(content, cursor .. content.high)
  of mdHLine:   skipAtNextLine(content, cursor .. content.high)

  of mdbPar:    skipAfterParagraphSep(content, cursor .. content.high)
  of mdbQuote:  skipAfterParagraphSep(content, cursor .. content.high)

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
  
  of mdbList:   cursor
  of mdbTable:  cursor
  else: 
    raise newException(ValueError, fmt"invalid block type '{kind}'")

proc stripContent(content: string, slice: Slice[int], kind: MdNodeKind): Slice[int] = 
  case kind
  of mdbMath:   stripSlice(content, slice, {'$', ' ', '\t', '\n', '\r'})
  of mdbCode:   stripSlice(content, slice, {'`', ' ', '\t', '\n', '\r'})
  of mdbHeader: skipChars(content, slice, {'#', ' '}) .. slice.b
  else: slice  


proc parseMdSpans(content: string, slice: Slice[int]): seq[MdNode] = 
  discard

proc parseMdBlock(content: string, slice: Slice[int], kind: MdNodeKind): MdNode = 
  let contentslice = stripContent(content, slice, kind)

  case kind
  
  of mdHLine: 
    MdNode(kind: mdHLine)
  
  of mdbHeader: 
    var b = MdNode(kind: mdbHeader, priority: contentslice.a-slice.a)
    # TODO now go for inline sub nodes
    # echo 'H' , b.priority, ' ', content[slice]
    b
  
  of mdbPar: 
    var b = MdNode(kind: mdbPar)
    # echo "(par) ", content[slice]
    # discard nextSpanCandidate(content, cursor)
    b

  of mdbMath: 
    var b = MdNode(kind: mdbMath)
    b
  
  of mdbCode: 
    # TODO detect lang (if provided)
    var b = MdNode(kind: mdbCode)
    b
  
  of mdbList: 
    var b = MdNode(kind: mdbList)
    b
  
  of mdbQuote:
    var b = MdNode(kind: mdbQuote)
    b
  
  # of mdbTable:
  #    discard
  
  else: 
    raise newException(ValueError, fmt"invalid block type '{kind}'")

proc parseMarkdown(content: string): MdNode = 
  result = MdNode(kind: mdWrap)

  var cursor = 0
  while cursor < content.len:
    let head = skipWhitespaces(content, cursor)
    let kind = detectBlockKind(content, head)
    let tail = afterBlock(content, head, kind)
    echo ":: ", kind, ' ', head .. tail, ' ', '[', content[head], ']', ' ', '<', content.substr(head, tail-1), '>'
    if tail - head <= 0: break
    let b    = parseMdBlock(content, head .. tail-1, kind)
    cursor = tail

 
# -----------------------------

# TODO auto link finder (convert normal text -> link)
#  TODO detect indent

when isMainModule:
  # echo startsWith("### hello", 0, p"#+\s+")
  # echo startsWith("# hello",   0, p"#+\s+")
  # echo startsWith("hello",     0, p"#+\s+")
  # echo startsWith("```py\n wow\n```", 0, p"```")
  # echo startsWith("-----", 0, p"---+")
  # echo startsWith("", 0, p"```")
# else:

  for (t, path) in walkDir "./tests/easy":
    if t == pcFile: 
      echo "------------- ", path

      let 
        content = readFile path
        doc     = parseMarkdown content
      
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
