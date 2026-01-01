import std/[
  unittest,
  os,
]
import ../src/md


suite "E2E":
  for (t, path) in walkDir "./tests/cases":
    if t == pcFile: 
      test path:
        let 
          content = readFile path
          doc     = parseMarkdown content
          newdoc  = attachNextCommentOfFigAsDesc doc
        
        writeFile "./play.xml", toXml newdoc
        writeFile "./play.tex", toTex newdoc
