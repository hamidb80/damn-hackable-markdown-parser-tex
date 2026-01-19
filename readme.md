# Damn Hackable Markdown Parser + Tex Convertor

## Motivation

> This would require a major API (and internals) rework though, 
> while we would also need to think about backwards compatibility - 
> so apparently not a trivial stuff. 
> 
> from: https://github.com/miyuchina/mistletoe/issues/257


we're in 2025, age of AI, 
and still there is not a single simple markdown parser that you can extend 
without quirks in Python or JS.

I hate these *noobie* developers that would say 
"this requires major blah blah" for *every single issue* instead of 
actually doing the **right work**. 

There are some other markdown parser libraries in other programming langugages
that have _huge code-base_, and they implement extensibility better. 
But I don't understand the force for making such a _small_ functionality (single parser) 
a separate **project**, and put lots of effort/design patterns into it. 
just write a simple and normal code and let anyone customize it.

I'm angry; becuase first of all, there is not a single markdown standard like in HTML or JSON
but dozens and they vary even in basic things, like the markdown produced by LLMs often use 
`\(` and `\)` for LaTex inside markdown instead of `$`. why write a huge code-base 
that enables extensiblilty (well, most of them can't even reach this level. 
like the one I've quoted) when you can't use it where you want? 

this is an attempt to end my struggles and headaches, 
IN JUST A SINGLE DAMN FILE. 
if you customize it, just grab and change the source code.

## a message to those noobie devs

please read my code and learn. 
you don't need 50+ classes and 10+ interfaces or 5+ dependencies to parse a markdown!
you just need to know how to program, 
do not over-complicate everything.
> "this requires major blah blah"

## Features

### Parser

the parser detects language changes and stores texts inside a `dir` node.
it is specially useful for rtl languages like Arabic. the behaviour can be disabled.

#### inline elements
- bold `**`
- italic `_`
- inline code `\``
- inline math `$`
- comment `// ...`
- wiki links `[[...]]`
- links `[...]()`
- embeds `![...]()`

#### blocks
- paragraph
- code block triple `\``
- math block `$$`
- lists (numberical and `+` `-` `*`)
- wiki embeds `![[...]]`


## TODO
- [ ] nested blocks (like lists)
- [ ] indentations
- [ ] table


## Usage

run `src/cli.nim`
