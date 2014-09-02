namespace Haskell

module Tokenize = 
    val toTokens : String -> Token list
    val toLines : Token list -> Token list list