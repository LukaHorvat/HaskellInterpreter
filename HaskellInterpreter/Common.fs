namespace Haskell

[<AutoOpen>]
module Types =
    type String = char list
    
    type TokenType = string

    type LineColumn = int * int

    type Token = 
        | Token of TokenType * String * LineColumn

    type AST = 
        | Sequence of AST list
        | Node of AST * Token * AST
        | Leaf of Token

    [<Literal>]
    let TabLength = 4

    type IndentationTree = 
        | Expression of Token list * IndentationTree list
        | IndentList of IndentationTree list
        | Statement of Token list