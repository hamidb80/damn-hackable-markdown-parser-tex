import std/[os, strformat, strutils]
import md

proc pkgVersion(nimbleContent: string): string = 
  let 
    i       = nimbleContent.find('"')
    s       = nimbleContent.find('"', i+1)
    version = nimbleContent[i+1 .. s-1] # -d:NimblePkgVersion how to add this?
  return version



const
  version      = pkgVersion slurp "../md.nimble"
  persian_flag = "persian"


proc convertFile(ipath, opath: string, settings: MdSettings, persian: bool) =
  echo fmt">> {ipath} -> {opath}"
  let
    (idir, ifile, iext) = splitFile ipath
    (odir, ofile, oext) = splitFile opath
    content             = readfile ipath

  var md = MdNode(
      kind: mdWrap, 
      children: @[
        MdNode(kind: mdComment, content:  fmt"generated from: {ipath}"), 
        MdNode(kind: mdChapter, children: parseParMdSpans(ifile)),
      ])
  
  parseMarkdown(content, md)
  md = attachNextCommentOfFigAsDesc md

  if persian:
    md = persianContVerbFixer md

  let
    result   =
      case oext.toLowerAscii
      of ".tex":  toTex md, settings
      of ".json": toJson md
      else:       quit fmt"invalid output file extension '{oext}', see help"

  try:    writeFile opath, result
  except: quit fmt"cannot write output file at '{opath}'"


when isMainModule:
  if paramCount() >= 4:
    let
      dir   = paramStr 1
      pw    = paramStr 2
      ipath = paramStr 3
      opath = paramStr 4
      extra = commandLineParams()[4..^1]

    var 
      feat_persian = false      
    

    for flag in extra:
      case flag
      of persian_flag:
        feat_persian = true
      else:
        quit fmt"invalid flag {flag}"
      
      echo fmt"++ '{flag}'"

    let
      (idir, iname, iext) = splitFile ipath
      (odir, oname, oext) = splitFile opath
      
      pagewidth     = 
        try:    parseint pw
        except: quit fmt"invalid page width '{pw}', see help"
      
      textdirection = 
        case dir.toLowerAscii
        of "ltr":   mddLtr
        of "rtl":   mddRtl
        of "nodir": mddUndecided
        else      : quit fmt"invalid '{dir}' direction, see help"
      
      settings      = MdSettings(pagewidth: pagewidth, langdir: textdirection)

    if iext.toLowerAscii != ".md":
      quit fmt"invalid input file extension '{iext}', see help"

    if iname == "*":
      echo idir
      for ftype2, ipath2 in walkDir(idir, false, true):
        if ftype2 == pcFile:
          let (_, name, ext) = splitFile ipath2
          if ext.toLowerAscii == ".md":
            convertFile ipath2, odir / name & oext, settings, feat_persian
    else:
      convertFile ipath, opath, settings, feat_persian

  else:
    quit dedent fmt"""
      === Damn Markdown Parser === 
      v{version}

      USAGE:
         app LANG_DIR PAGE_WIDTH path/to/file.md path/to/file.EXT ...FLAGS

      SURPRISE:
        you can use path/to/*.md to capture all .md files in the directory

      WHERE:
        LANG_DIR   `ltr` or `rtl` or `nodir`
        PAGE_WIDTH integer number. according to this parameter, the width of images are set
        EXT        `tex` or `json`
        FLAGS
          * {persian_flag}: fixes some common persian tokens
    """
