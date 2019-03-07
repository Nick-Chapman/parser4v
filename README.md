# parser4v
Haskell implementation of 4-value parser combinators.Based on the classic paper:

Andrew Partridge, David Wright:
    Predictive parser combinators need four values to report errors.
    Journal of Functional Programming 6(2): 355-364, 1996

This repo contain a Haskell implementation of combinators, coded in both the 4-way variant style, and also the 4-continuation style. And an example of the use of the combinators to encode a parser for a small expression language.

- par4v.hs    -- combinators in 4-way variant style
- par4c.hs    -- combinators in 4-continuation style
- ast.hs      -- AST for small expression language
- syntax.hs   -- parser for the expression language using the combinators (can use either combinator style)
- tests.hs    -- unit tests for parser (success and failure)
- main.hs     -- run the unit tests


Build/run:
- $ ghc -Wall -fno-warn-name-shadowing --make result par4v par4c ast syntax tests main -o main.exe
- $ main.exe
