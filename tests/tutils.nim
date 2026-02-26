import std/[unittest, options]
import md


suite "Utils":

  test "startsWith":
    check 4        == startsWith("### hello",        0, p"#+\s+")
    check 2        == startsWith("# hello",          0, p"#+\s+")
    check notfound == startsWith("hello",            0, p"#+\s+")
    check 3        == startsWith("```py\n wow\n```", 0, p"```")
    check 4        == startsWith("\n```",            0, p "\n```")
    check 5        == startsWith("-----",            0, p"---+")
    check 3        == startsWith("\n$$",             0, p "\n$$")
    check 3        == startsWith("4. hi",            0, p"\d+. ")
    check 4        == startsWith("43. hi",           0, p"\d+. ")
    check notfound == startsWith("",                 0, p"\d+. ")
    check notfound == startsWith("**",               0, p"* ")



suite "Functionality":

  test "getWikiLabel":
    check "SVD" == getWikiLabel "content/linear algebra/SVD"
    check "SVD" == getWikiLabel "linear algebra/SVD"
    check "SVD" == getWikiLabel "SVD"
    check "linear algebra" == getWikiLabel "content/linear algebra"
    check "singular value decomposition" == getWikiLabel "linear algebra/SVD | singular value decomposition"

  test "getWikiEmbedSize":
    check 400 == getWikiEmbedSize "assets/image.png| 400"
    check 400 == getWikiEmbedSize "assets/image.png | 400"
    check 400 == getWikiEmbedSize " assets/image.png |400"
    check   0 == getWikiEmbedSize "assets/image.png"
    check   0 == getWikiEmbedSize "image.png"

  test "getWikiPath":
    check "content/linear algebra/SVD" == getWikiPath " content/linear algebra/SVD "
    check "assets/image.png" == getWikiPath " assets/image.png |400"


suite "Tex":

  test "writeEscapedTex":
    var str = ""
    writeEscapedTex r"you^re 50% of _me :-\\", str
    check str ==    r"you\^re 50\% of \_me :-\\\\"
