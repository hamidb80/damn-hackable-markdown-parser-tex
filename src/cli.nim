import std/[os, strformat]
import md


when isMainModule:
  case paramCount()
  of 3:
    let
      d = paramStr 1
      textdirection = 
        case d
        of "ltr": mddLtr
        of "rtl": mddRtl
        else    : raise newException(ValueError, fmt"invalid '{d}' direction, direction can only be `ltr` or `rtl`")
      ipath = paramStr 2
      opath = paramStr 3
      (_,_, oext) = splitFile opath
      settings    = MdSettings(pagewidth: 1000, langdir: textdirection)
      md          = attachNextCommentOfFigAsDesc parseMarkdown readFile ipath
    
    case oext
    of ".tex":
      writeFile opath, toTex(md, settings)
    of ".xml":
      writeFile opath, toXml md
    else:
      quit "only `.tex` and `.xml` output file extensions are supported"

  else:
    quit """
      USAGE:
         app DIR path/to/file.md path/to/file.EXT

      WHERE:
        EXT can be `tex` or `xml`
        DIR can be `ltr` or `rtl`
    """
