# Input
The input is an old documentation in a kind of very baby nroff.
# Requirement
We wish to convert it to a modern format like html
We dont even want 100% correct conversion; if 80% is handled correctly and the remaining 20% is somewhat garbled it can be hand edited later
# Format
The only nroff we need to handle is
## Headers
.ST n.n.n... Header...
'.' in 1st char of the line; the number of ns determine the heading level; the remaining Header words are the heading at that level
Other '.' commands can be ignored
## Blocks
The main formatting that is in the input literally (not as nroff commands) is bullet lists and verbatim blocks
### Bullet Lists
Bullet lists are like
  o  autem amet explicabo iusto necessitatibus
Or a nested bullet
       o  deleniti vero minus at et 

Verbatim blocks are indented by some non zero amount
Verbatim blocks can nest in bullets but not vice versa
[The current haskell attempt allows both directions nesting just to make it more regular and easier]
.ST commands close all pending indents
Other dot commands have no effect
# Notes
Clearly the input is indentation sensitive like Python or Haskell
The current haskell attempt at a lexer is to grok the indentation context and to digest and reify and emit explicit open and close tokens for bullets and verbs 
