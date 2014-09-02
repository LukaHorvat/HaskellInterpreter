namespace Haskell

module AST =
    val toIndentTree : Token list -> IndentationTree
    val fromTokens : Token list -> AST
