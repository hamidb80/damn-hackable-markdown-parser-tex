import std/[
  unittest,
]
import ../src/md

suite "Utils":
  test "startsWith":
    check 4        == startsWith("### hello", 0, p"#+\s+")
    check 2        == startsWith("# hello",   0, p"#+\s+")
    check notfound == startsWith("hello",     0, p"#+\s+")
    check 3        == startsWith("```py\n wow\n```", 0, p"```")
    check 4        == startsWith("\n```", 0, p "\n```")
    check 5        == startsWith("-----", 0, p"---+")
    check 3        == startsWith("\n$$",  0, p "\n$$")
    check 3        == startsWith("4. hi",  0, p"\d+. ")
    check 4        == startsWith("43. hi", 0, p"\d+. ")
    check notfound == startsWith("",       0, p"\d+. ")
    check notfound == startsWith("**",     0, p"* ")
