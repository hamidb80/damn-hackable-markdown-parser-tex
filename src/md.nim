import std/[
  strutils, strformat, 
  lists, sequtils,
  tables,
  options,
  os,
]


type
  MdNodeKind = enum
    # wrapper
    mdWrap # XXX add indent to this

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
    mdsBold ## **...**
    mdsItalic ## *...* _..._
    mdsHighlight ## ==...==
    mdsCode ## `...`
    mdsMath ## $...$
    mdsLink ## ![]()
    mdsEmbed ## ![]()
    mdsWikilink ## [[ ... ]]
    mdsComment ## // ...
    mdsWikiEmbed ## like photos, PDFs, etc ![[...]]
    mdsText ## processed text
    # mdsTag ## #...

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

const notfound = -1

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
  of mdbMath:   stripSlice(content, slice, {'$'} + Whitespace)
  of mdbCode:   stripSlice(content, slice, {'`'} + Whitespace)
  of mdbHeader: stripSlice(content, slice, {'#'} + Whitespace)
  of mdbQuote:  stripSlice(content, slice, {'>'} + Whitespace)
  of mdbPar:    stripSlice(content, slice, Whitespace)
  else: slice

proc replace[T](list: var DoublyLinkedList[T], n: DoublyLinkedNode[T], left, right: T) = 
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

proc replace[T](list: var DoublyLinkedList[T], n: DoublyLinkedNode[T], repl: T) = 
  let r = newDoublyLinkedNode(repl)
  r.prev = n.prev
  r.next = n.next

  let h = n == list.head
  let t = n == list.tail

  if h: list.head = r
  if t: list.tail = r

proc subtract[int](n, m: Slice[int]): seq[Slice[int]] = 
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

  if n.a < m.a: # start only
    result.add n.a .. m.a-1
  
  if n.b > m.b: # end only
    result.add m.b+1 .. n.b

proc scrabbleMatchDeep(content: string, indexes: var DoublyLinkedList[Slice[int]], pattern: string): Option[Slice[int]] =
  var j = 0
  var n: DoublyLinkedNode[Slice[int]]

  block find:
    for ni in indexes.nodes:
      let cindexes = ni.value # consequtive indexes
      for i in cindexes:
        if pattern[j] == content[i]:
          inc j
          if j == pattern.len: 
            result = some i-j+1 .. i
            n = ni
            break find

        else:
          reset j

  # no change the indexes
  if not isNil n:
    let subs = subtract(n.value, result.get)
    case subs.len
    of 1: replace(indexes, n, subs[0])
    of 2: replace(indexes, n, subs[0], subs[1])
    else: raise newException(ValueError, "invalid subs")

proc scrabbleMatchDeepMulti(content: string, indexes: var DoublyLinkedList[Slice[int]], pattern: seq[string]): Option[seq[Slice[int]]] = 
  var acc: seq[Slice[int]]

  # TODO try not to manipulate indexes here
  for p in pattern:
    let match = scrabbleMatchDeep(content, indexes, p)
    if issome match:
      acc.add match.get
    else:
      break

  if   acc.len == 0: 
    return
  elif acc.len == pattern.len:
    return some acc
  else:
    raise newException(ValueError, "cannot match")

proc parseMdSpans(content: string, slice: Slice[int]): seq[MdNode] = 
  var indexes = toDoublyLinkedList([slice])

  for k in [
    # sorted by priority
    mdsCode,
    mdsMath,
    mdsBold,
    mdsItalic,
    mdsHighlight,
    mdsLink,
    mdsEmbed,
    mdsWikilink,
    mdsComment,
    mdsWikiEmbed,
    mdsText,
  ]:
    # TODO support escape \ in patterns

    proc matchPairInside(l, r: string): Option[Slice[int]] = 
      let r = scrabbleMatchDeepMulti(content, indexes, @[l, r])
      if isSome r:
        let bounds = r.get
        let span = bounds[0].b+1 .. bounds[1].a-1
        result = some span
      
    while true:
      case k 
      of mdsBold: 
        let v = matchPairInside("**", "**")
        if issome v: echo (k, v.get)
        else: break
        

      of mdsItalic:
        #  TODO "*" .. "*"
        let v = matchPairInside("_", "_")
        if issome v: echo (k, v.get)
        else: break

      of mdsHighlight:
        let v = matchPairInside("==", "==")
        if issome v: echo (k, v.get)
        else: break

      of mdsCode:
        let r = scrabbleMatchDeepMulti(content, indexes, @["`", "`"])
        if isSome r:
          let bounds = r.get
          indexes.subtract bounds[0].b+1 .. bounds[1].a-1

      of mdsMath:
        discard # TODO like above
        # let r = scrabbleMatchDeepMulti(content, indexes, @["$", "$"])
        # if isSome r:
        #   let bounds = r.get

      of mdsWikiEmbed:
        let v = matchPairInside("![[", "]]")
        if issome v: echo (k, v.get)
        else: break

      of mdsWikilink:
        let v = matchPairInside("[[", "]]")
        if issome v: echo (k, v.get)
        else: break

      # of mdsEmbed:
      #   let r = scrabbleMatchDeepMulti(content, indexes, @["![", "](", ")"])
      #   if isSome r:
      #     let bounds = r.get
      #     let span = bounds[0].b+1 .. bounds[1].a-1
      #     echo (k, span)
      #   else:
      #     break

      # of mdsLink:
      #   let r = scrabbleMatchDeepMulti(content, indexes, @["[", "](", ")"])
      #   if isSome r:
      #     let bounds = r.get
      #     let span = bounds[0].b+1 .. bounds[1].a-1
      #     echo (k, span)
      #   else:
      #     break

      # of mdsComment:
      #   let r = scrabbleMatchDeepMulti(content, indexes, @["//", "$"])
      #   if isSome r:
      #     let bounds = r.get
      #     let span = bounds[0].b+1 .. bounds[1].a-1
      #     echo (k, span)
      #   else:
      #     break

      of mdsText:
        for i in indexes: # all other non matched scrabbles
          # TODO remove scape characters here
          echo (mdsText, i)
        break

      else: 
        break

  # aggregate

proc parseMdBlock(content: string, slice: Slice[int], kind: MdNodeKind): MdNode = 
  let contentslice = stripContent(content, slice, kind)
  echo content[contentslice]

  case kind
  
  of mdHLine: 
    MdNode(kind: mdHLine)
  

  of mdbHeader: 
    MdNode(kind: mdbHeader, 
           priority: skipChar(content, slice, '#') - slice.a,
           children: parseMdSpans(content, contentslice))
  
  of mdbPar: 
    MdNode(kind: mdbPar, 
           children: parseMdSpans(content, contentslice))

  of mdbQuote:
    MdNode(kind: mdbQuote, 
           children: parseMdSpans(content, contentslice))
  

  of mdbMath: 
    MdNode(kind: mdbMath, content: content[contentslice])
  
  of mdbCode: 
    # TODO detect lang (if provided)
    MdNode(kind: mdbMath, content: content[contentslice])

  of mdbList: 
    var b = MdNode(kind: mdbList)
    b
  
  else: 
    raise newException(ValueError, fmt"invalid block type '{kind}'")

proc parseMarkdown(content: string): MdNode = 
  result = MdNode(kind: mdWrap)

  var cursor = 0
  while cursor < content.len:
    let head = skipWhitespaces(content, cursor)
    let kind = detectBlockKind(content, head)
    let tail = afterBlock(content, head, kind)
    echo ":: ", kind, ' ', head .. tail, ' ', '[', content[head], ']'
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

#   const t = "wow how are you man??"
#   var indexes = toDoublyLinkedList([0..<t.len])
#   let res = scrabbleMatchDeep(t, indexes, "are")
#   echo ':', t[res.get], ':', indexes
# else:
  for (t, path) in walkDir "./tests/easy":
    if t == pcFile: 
  # block: 
  #     let path = "./tests/easy/play.md"
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
