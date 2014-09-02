namespace UnitTestProject1

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Haskell
open System.IO

[<TestClass>]
type ParserTest() = 
    
    [<TestMethod>]
    member x.Tokenizing() = 
        let source = @"a + ""h\""ello""if"
        let tokens = Tokenize.toTokens (List.ofSeq source)
        Assert.AreEqual([ Token("identifier", [ 'a' ], (0, 0))
                          Token("whitespace", [ ' ' ], (0, 1))
                          Token("operator", [ '+' ], (0, 2))
                          Token("whitespace", [ ' ' ], (0, 3))
                          Token("string", List.ofSeq "\"h\\\"ello\"", (0, 4))
                          Token("keyword", [ 'i'; 'f' ], (0, 13)) ], tokens)
    
    [<TestMethod>]
    member x.Lines() = 
        let source = @"a
b
c"
        
        let lines = 
            List.ofSeq source
            |> Tokenize.toTokens
            |> Tokenize.toLines
        Assert.AreEqual(List.head lines |> List.head, Token("identifier", [ 'a' ], (0, 0)))

    [<TestMethod>]
    member x.IndentTree() =
        let source = @"fact n
    | n == 0 = 1
    | n == 1 = 1
    | otherwise = n * fact (n - 1)

const x = 0"
        let tree = List.ofSeq source |> Tokenize.toTokens |> AST.toIndentTree
        ()
