import std/[os, strformat, strutils]
import md


when isMainModule:
  case paramCount()
  of 4:
    let
      dir   = paramStr 1
      pw    = paramStr 2
      ipath = paramStr 3
      opath = paramStr 4
      
      (_,_, iext) = splitFile ipath
      (_,_, oext) = splitFile opath
      
      pagewidth     = 
        try:    parseint pw
        except: quit fmt"invalid page width '{pw}', see help"
      textdirection = 
        case dir.toLowerAscii
        of "ltr":   mddLtr
        of "rtl":   mddRtl
        of "nodir": mddRtl
        else      : quit fmt"invalid '{dir}' direction, see help"
      settings      = MdSettings(pagewidth: pagewidth, langdir: textdirection)
      
      content  = 
        case iext.toLowerAscii
        of ".md":
          try:    readFile ipath
          except: quit fmt"cannot read input file at '{ipath}'"
        else:     quit fmt"invalid input file extension '{iext}', see help"
      md       = attachNextCommentOfFigAsDesc parseMarkdown content
      result   =
        case oext.toLowerAscii
        of ".tex": toTex md, settings
        of ".xml": toXml md
        else:      quit fmt"invalid output file extension '{oext}', see help"

    try:    writeFile opath, result
    except: quit fmt"cannot write output file at '{opath}'"

  else:
    quit """
      USAGE:
         app LANG_DIR PAGE_WIDTH path/to/file.md path/to/file.EXT

      WHERE:
        LANG_DIR   `ltr` or `rtl` or `nodir`
        PAGE_WIDTH integer number. according to this parameter, the width of images are set
        EXT        `tex` or `xml`
    """
