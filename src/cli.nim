import std/[os]
import md


when isMainModule:
  case paramCount()
  of 2:
    let
      ipath = paramStr 1
      opath = paramStr 2
      (_,_, oext) = splitFile opath
      settings    = MdSettings(pagewidth: 1000)
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
         app path/to/file.md path/to/file.tex
    """
