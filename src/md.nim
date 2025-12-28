import std/[
  strutils,
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

  SkipWhitespaceReport = object
    counts: array[0 .. 128, int]

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

proc parseMdBlock(content: string, slice: Slice[int]): Option[MdNode] = 
  # TODO detect indent

  ## detect type of block
  # if code block
  # if math block
  # if table
  # if quote
  # if 
  # if list
  # if header
  # else (par) 

  discard nextSpanCandidate(content, slice.a)


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
