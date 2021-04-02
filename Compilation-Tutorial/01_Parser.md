# Parsing

Befor coding, let's get through some crucial definitions.

- A **token** is the basic component of source code, for example constant, identifier, operator, a reserved word, etc.
- A **parse tree** is an ordered rooted tree that represents the syntactic structure of a string according to some context-free grammar.
- An **abstract syntax tree** (usually just referred to as an AST) is really nothing more than a simplified condensed version of a parse tree.

Parsing is brokend down into two parts:

- lexing : converting a stream of characters into a stream of tokens,
- parsing : converting a stream of tokens into the final representation, the form of a tree-like data structure called an abstract syntax tree.


## Defining a parser

For now, we need a file with suffix .mly whihc will be filled with parser specification. It ought to consist two sections.

