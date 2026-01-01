import std/[
  unittest,
  os,
  strformat,
]
import md


suite "E2E":
  for (t, path) in walkDir "./tests/cases":
    if t == pcFile: 
      test path:
        try:
          let 
            content = readFile path
            doc     = parseMarkdown content
            newdoc  = attachNextCommentOfFigAsDesc doc
            fname   = path.splitFile.name

          writeFile fmt"./dist/{fname}.xml", toXml newdoc
          writeFile fmt"./dist/{fname}.tex", toTex newdoc

        except CatchableError:
          check false