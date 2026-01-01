import std/[
  strutils, strformat, 
  lists, sequtils,
  algorithm,
  options,
  os,
]


type
  MdNodeKind = enum
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
    mdsLink ## ![]()
    mdsEmbed ## ![]()
    mdsWikilink ## [[ ... ]]
    mdsComment ## // ...
    mdWikiEmbed ## like photos, PDFs, etc ![[...]]
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
    numbered: bool   # for list
    dir:      MdDir  # for text
    priority: int    # for header
    lang:     string # for code
    href:     string # for link


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

# --------------------------------------------

const notfound = -1

# --------------------------------------------

const MdLeafNodes = {mdsText, 
                    mdsMath, mdsCode, 
                    mdsEmbed, mdWikiEmbed, mdsWikilink, 
                    mdbMath, mdbCode}

func empty(a: seq): bool = a.len == 0

func toXml(n: MdNode, result: var string) = 
  result.add "<"
  result.add $n.kind
  result.add ">"

  case n.kind
  of MdLeafNodes: result.add n.content
  else          : discard

  for sub in n.children:
    toXml sub, result

  result.add "</"
  result.add $n.kind
  result.add ">"

func toXml(n: MdNode): string = 
  toXml n, result


func toTex(n: MdNode, result: var string) = 
  case n.kind

  of mdWrap:
    for sub in n.children:
      toTex sub, result
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
    for sub in n.children:
      toTex sub, result
    result.add '}'

  of mdbPar:
    for sub in n.children:
      toTex sub, result
  
  # TODO
  of mdsDir: 
    # \usepackage{bidi}
    # \lr : ltr 
    # \rl : rtl
    result.add "\\lr{"
    for sub in n.children:
      toTex sub, result
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
    # let repl = MdNode(kind: mdsBold, 
    #                  children: @[MdNode(kind: mdsItalic, 
    #                                     children: n.children)])
    # toTex repl, result
    result.add "\\verb{"
    for sub in n.children:
      toTex sub, result
    result.add "}"

  of mdsBold: 
    result.add "\\textbf{"
    for sub in n.children:
      toTex sub, result
    result.add "}"

  of mdsItalic: 
    result.add "\\textit{"
    for sub in n.children:
      toTex sub, result
    result.add "}"

  of mdsHighlight: 
    result.add "\\hl{"
    for sub in n.children:
      toTex sub, result
    result.add "}"

  of mdsComment:
    result.add "\\begin{small}"
    for sub in n.children:
      toTex sub, result
    result.add "\\end{small}"

  of mdsText: 
    result.add n.content

  of mdHLine: 
    result.add "\\clearpage"

  of mdsWikilink: 
    toTex MdNode(kind: mdsItalic, children: @[MdNode(kind: mdbPar, children: @[
      MdNode(kind: mdsText, content: "WIKILINK")
    ])]), result

  of mdWikiEmbed:
    let size = 15

    result.add "\\begin{figure}[H]\n"
    result.add "\\centering\n"
    result.add "\\includegraphics[width=" 
    result.add $size
    result.add "cm,keepaspectratio]{"
    result.add n.content
    result.add "}\n"
    result.add "\\caption{"
    for sub in n.children:
      toTex sub, result
    result.add "}\n"
    result.add "\\end{figure}"

  of mdsEmbed:
    # TODO
    raise newException(ValueError, fmt"TODO")

  of mdFrontMatter:
    discard

  of mdbList:
    let tag = 
      if n.numbered: "enumerate"
      else:          "itemize"
    result.add "\\begin{"
    result.add tag
    result.add "}\n"
    for sub in n.children:
      result.add "\\item "
      toTex sub, result
    result.add "\n\\end{"
    result.add tag
    result.add "}"

  of mdbTable, mdsLink, mdbQuote:
    raise newException(ValueError, fmt"toTex for kind {n.kind} is not implemented")

func toTex(n: MdNode): string = 
  toTex n, result


func at(str: string, index: int): char = 
  if index in str.low .. str.high: str[index]
  else:                            '\0'

proc p(pattern: string): SimplePattern = 
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

proc matches(ch: char, pt: SimplePatternToken): bool = 
  case pt.kind
  of sptChar: ch == pt.ch
  of sptMeta:
    case pt.meta
    of spmWhitespace: ch in Whitespace
    of spmDigit     : ch in Digits

const listPatterns = [p"- ", p"\+ ", p"* ", p "\\d+. "]

proc find(content: string, slice: Slice[int], sub: string): int = 
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

proc skipWhitespaces(content: string, cursor: int): int = # : SkipWhitespaceReport = 
  var i = cursor
  while i < content.len:
    if content[i] in Whitespace:
      inc i
    else:
      break
  i

proc startsWith(str: string, cursor: int, pattern: SimplePattern): int = 
  if str.high < cursor: return notfound

  var 
    j = 0      # current pattern
    c = 0      # count
    i = cursor # string index

  while true:
    if j == pattern.len: # all sub patterns satisfied
      return i

    if i >= str.len: 
      # if j == pattern.high and c in pattern[j].repeat:
      #   return i
      # else:
        break
    
    # let cond = matches(str[i], pattern[j].token)
    # echo (i, j, c, str[i], pattern[j], cond)

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

proc skipBefore(content: string, cursor: int, pattern: SimplePattern): int = 
  var i  = cursor
  
  while i < content.len:
    if startsWith(content, i, pattern) != notfound: return i-1
    inc i
  
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
  if   startsWith(content, cursor, p"```")   != notfound: mdbCode
  elif startsWith(content, cursor, p"$$\s")  != notfound: mdbMath
  elif startsWith(content, cursor, p"> ")    != notfound: mdbQuote
  elif startsWith(content, cursor, p"#+ ")   != notfound: mdbHeader
  elif startsWith(content, cursor, p"---+")  != notfound: mdHLine
  elif startsWith(content, cursor, p"![[")   != notfound: mdWikiEmbed
  elif startsWith(content, cursor, p"![")    != notfound: mdsEmbed
  elif listPatterns.anyit(notfound != startsWith(content, cursor, it)): mdbList
  else: mdbPar

proc skipAfterParagraphSep(content: string, slice: Slice[int], kind: MdNodeKind): int = 
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

proc afterBlock(content: string, cursor: int, kind: MdNodeKind): int = 
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

proc stripContent(content: string, slice: Slice[int], kind: MdNodeKind): Slice[int] = 
  case kind
  of mdbMath:      stripSlice(content, slice, {'$'} + Whitespace)
  of mdbCode:      stripSlice(content, slice, {'`'})
  of mdbHeader:    stripSlice(content, slice, {'#'} + Whitespace)
  of mdbQuote:     stripSlice(content, slice, {'>'} + Whitespace)
  of mdbPar:       stripSlice(content, slice, Whitespace)
  of mdWikiEmbed:  stripSlice(content, slice, {'!', '[', ']'} + Whitespace)
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
  n.value = repl

proc subtract(n, m: Slice[int]): seq[Slice[int]] = 
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

proc intersects(n, m: Slice[int]): bool =
  subtract(n, m) != @[n]

proc contains(n, m: Slice[int]): bool =
  m.a in n and 
  m.b in n

proc subtract(ns: var DoublyLinkedList[Slice[int]], n: DoublyLinkedNode[Slice[int]], m: Slice[int]) = 
  let subs = subtract(n.value, m)
  case subs.len
  of 0: ns.remove n
  of 1: replace(ns, n, subs[0])
  of 2: replace(ns, n, subs[0], subs[1])
  else: raise newException(ValueError, "invalid subs: " & $subs.len)

proc `+`(n,m: Slice[int]): Slice[int] = 
  (n.a + m.a) .. (n.b + m.b)

proc scrabbleMatchDeep(content: string, indexes: var DoublyLinkedList[Slice[int]], pattern: string): Option[Slice[int]] =
  var j = 0
  var n: DoublyLinkedNode[Slice[int]]

  block find:
    for ni in indexes.nodes:
      let area = ni.value # consequtive indexes
      for i in area:
        let cond = (i == 0 or content[i-1] != '\\') and # considers escape
           pattern[j] == content[i]
        if (i == 0 or content[i-1] != '\\') and # considers escape
           pattern[j] == content[i]: 
          inc j
          if j == pattern.len: 
            result = some i-j+1 .. i
            n = ni
            break find

        else:
          reset j

  # no change the indexes
  if not isNil n:
    subtract indexes, n, result.get

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
    # echo scrabbleMatchDeep(content, indexes, pattern[0])
    raise newException(ValueError, "cannot match")

proc parseMdSpans(content: string, slice: Slice[int]): seq[MdNode] = 
  var acc: seq[tuple[kind: MdNodeKind, slice: Slice[int]]]
  var indexes = toDoublyLinkedList([slice])

  for k in [
    # sorted by priority
    mdsDir,
    mdsCode,
    mdsMath,
    mdWikiEmbed,
    mdsWikilink,
    mdsEmbed,
    mdsLink,
    mdsBoldItalic,
    mdsBold,
    mdsItalic,
    mdsHighlight,
    mdsComment,
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

      of mdsDir:
        break # TODO

      of mdsBoldItalic: 
        let v = matchPairInside("***", "***")
        if issome v: acc.add (k, v.get)
        else: break

      of mdsBold: 
        let v = matchPairInside("**", "**")
        if issome v: acc.add (k, v.get)
        else: break

      of mdsItalic:
        #  TODO "*" .. "*"
        let v = matchPairInside("_", "_")
        if issome v: acc.add (k, v.get)
        else: break

      of mdsHighlight:
        let v = matchPairInside("==", "==")
        if issome v: acc.add (k, v.get)
        else: break

      of mdsWikilink:
        let r = scrabbleMatchDeepMulti(content, indexes, @["[[", "]]"])
        if isSome r:
          let bounds = r.get
          let area = bounds[0].b+1 .. bounds[1].a-1
          acc.add (k, area)
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
          acc.add (k, area)
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
          acc.add (k, area)
          for ni in indexes.nodes:
            if ni.value.intersects area:
              indexes.subtract ni, area
        else:
          break

      # of mdsEmbed:
      #   let r = scrabbleMatchDeepMulti(content, indexes, @["![", "](", ")"])
      #   if isSome r:
      #     let bounds = r.get
      #     let span = bounds[0].b+1 .. bounds[1].a-1
      #   else:
      #     break

      # of mdsLink:
      #   let r = scrabbleMatchDeepMulti(content, indexes, @["[", "](", ")"])
      #   if isSome r:
      #     let bounds = r.get
      #     let span = bounds[0].b+1 .. bounds[1].a-1
      #   else:
      #     break

      of mdsComment:
        let head = scrabbleMatchDeep(content, indexes, "// ")
        if issome head:
          let match = scrabbleMatchDeep(content, indexes, "\n")
          let tail = 
            if issome match: match.get.b-1
            else: slice.b
          let span = (head.get.b + 1) .. tail
          acc.add (k, span)
        else:
          break


      of mdsText:
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
                  acc.add (k, cur.a .. i )
                  cur = i+2 .. cur.b

                else:
                  acc.add (k, cur.a ..< i )
                  cur = i+1 .. cur.b

                again = true
                break
          
          acc.add (k, cur)

        break

      else: 
        break

  # aggregate
  proc cmpFirst(a,b: (MdNodeKind, Slice[int])): int = 
    cmp(a[1].a, b[1].a)

  acc.sort cmpFirst
  # echo acc

  var root = MdNode(kind: mdWrap) # XXX define wrap
  var stack: seq[tuple[node: MdNode, slice: Slice[int]]] = @[(root, slice)]

  for c in acc:

    let node = 
      case c.kind
      of MdLeafNodes: 
        MdNode(kind: c.kind, 
               content: content[c.slice])
      else:
        MdNode(kind: c.kind)

    while true:
      if c.slice in stack[^1].slice:
        stack[^1].node.children.add node
        
        case node.kind
        of MdLeafNodes: discard
        else:           stack.add (node, c.slice)
        break

      else:
        discard stack.pop

  root.children

proc parseMdBlock(content: string, slice: Slice[int], kind: MdNodeKind): MdNode = 
  let contentslice = stripContent(content, slice, kind)

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
    # echo (langslice, content[langslice].strip, content[codeslice].strip)
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
                            children: parseMdSpans(content, s))

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
    if tail - head <= 0: break
    let b    = parseMdBlock(content, head .. tail-1, kind)
    result.children.add b
    cursor = tail

proc preprocess(root: sink MdNode): MdNode = 
  case root.kind
  of mdWrap:

    var newChildren: seq[MdNode]
    var i = 0

    # move comment just after an image to its description
    while i < root.children.len:
      newChildren.add root.children[i]

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

 
# -----------------------------

# TODO auto link finder (convert normal text -> link)

when isMainModule:
  # tests ---------------------------
  assert 4        == startsWith("### hello", 0, p"#+\s+")
  assert 2        == startsWith("# hello",   0, p"#+\s+")
  assert notfound == startsWith("hello",     0, p"#+\s+")
  assert 3        == startsWith("```py\n wow\n```", 0, p"```")
  assert 4        == startsWith("\n```", 0, p "\n```")
  assert 5        == startsWith("-----", 0, p"---+")
  assert 3        == startsWith("\n$$",  0, p "\n$$")
  assert 3        == startsWith("4. hi",  0, p"\d+. ")
  assert 4        == startsWith("43. hi", 0, p"\d+. ")
  assert notfound == startsWith("",       0, p"\d+. ")
  assert notfound == startsWith("**",     0, p"* ")

#   const t = "wow how are you man??"
#   var indexes = toDoublyLinkedList([0..<t.len])
#   let res = scrabbleMatchDeep(t, indexes, "are")
#   echo ':', t[res.get], ':', indexes

# else:
  for (t, path) in walkDir "./tests/":
    if t == pcFile: 
  # block: 
      # let path = "./tests/hard/reg.md"
      echo "------------- ", path

      let 
        content = readFile path
        doc     = parseMarkdown content
        newdoc  = preprocess doc
      
      writeFile "./play.xml", toXml newdoc
      writeFile "./play.tex", toTex newdoc
