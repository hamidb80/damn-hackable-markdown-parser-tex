import std/[strutils, sequtils]


type 
  MdDir* = enum
    mddUndecided
    mddRtl
    mddLtr

  Phrase* = object
    dir: MdDir
    slice: Slice[int]

func isUnicode(ch: char): bool = 
  127 < ch.uint

proc detectLang(content: string, area: Slice[int]): MdDir =
  for i in area:
    if isUnicode    content[i]: return mddRtl
    if isAlphaAscii content[i]: return mddLtr
  return mddUndecided


proc wordSlices(content: string, area: Slice[int]): seq[Slice[int]] =
  var changes: seq[int]
  var last = true # was whitespace?

  for i in area:
    let w = content[i] in Whitespace
    if  w != last:
      changes.add i
    last = w

  if not last:
    changes.add area.b+1

  for i in countup(1, changes.high, 2):
    let head = changes[i-1]
    let tail = changes[i]-1
    result.add head..tail

proc meltSeq[T](elements: seq[T]): seq[Slice[int]] = 
  var j = 0

  for i in 1 ..< elements.len:
    if elements[j] != elements[i]:
      result.add j ..< i
      j = i

  result.add j ..< elements.len

proc separateLangs(content: string, area: Slice[int]): seq[Phrase] =
  let 
    ws        = wordSlices(content, area)
    langs     = ws.mapit(detectLang(content, it))
    langsMelt = meltSeq langs

  for i, lm in langsMelt:
    result.add Phrase(dir: langs[lm.a], slice: ws[lm.a].a .. ws[lm.b].b)

proc separateLangs(content: string): seq[Phrase] =
  separateLangs content, 0..<content.len

const 
  c1 = """
    سلام چطوری؟ K-Means رو بالاخره فهمیدی؟ خارجی ها به KNN میگن K Nearest Neightboards خخخ
  """

  c2 = """
    camera means دوربین in persian
  """

  c3 = """
    این عبارت out-of-sample دیگه چیه؟
  """
  
  c4 = """
    wow, 72-folds(*) مگه شهر هرفته؟ این-دیگه-چی?? what is this??
  """

  c5 = """این  هست --cool-flag ها"""

for content in [c1, c2, c3, c4, c5]:
  let phrases = separateLangs content
  for ph in phrases:
    echo ph.dir, " '", content[ph.slice], "' "