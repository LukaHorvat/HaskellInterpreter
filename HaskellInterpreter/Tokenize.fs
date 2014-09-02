namespace Haskell

open FSharpExt
open FSharpExt.Parse
open FSharpExt.StateParse

module Tokenize = 
    let newline = Parse.string "\r\n" ||| Parse.char '\r' ||| Parse.char '\n'
    let comment = 
        Parse.string "--" |>| Parse.ifBefore (Parse.noneOrMore (Parse.not newline)) (Parse.nothing ||| newline)
    
    let multilineComment = 
        StateParse.withStateFunc ((+) 1) (Parse.string "{-") |.|| StateParse.withStateFunc ((-) 1) (Parse.string "-}") |.|| (fun state str -> 
        if state > 0 then StateParse.any state str
        else None)
        |> StateParse.oneOrMore
        |> StateParse.toStateless 0
    
    let number = 
        let exponent = Parse.anyOf "eE" |>| Parse.oneOrNone (Parse.anyOf "+-") |>| Parse.number
        (Parse.number |>| ((Parse.char '.' |>| Parse.number |>| Parse.oneOrNone exponent) ||| exponent)) 
        ||| (Parse.char '0' 
             |>| ((Parse.anyOf "oO" |>| Parse.oneOrMore (Parse.anyOf "01234567")) 
                  ||| (Parse.anyOf "xX" |>| Parse.oneOrMore (Parse.anyOf "0123456789abcdefABCDEF")))) ||| Parse.number
    
    let word = (Parse.letter |>| Parse.noneOrMore (Parse.word ||| Parse.digit ||| Parse.anyOf "'_"))
    let symbols = 
        [ '\\'; '-'; ':'; '.'; '_'; '!'; '#'; '$'; '%'; '&'; '*'; '+'; '/'; '<'; '='; '>'; '?'; '@'; '^'; '|'; '~' ]
    let isSymbol c = List.contains c symbols || System.Char.IsSymbol c
    let operator = Parse.oneOrMore (Parse.charCondition isSymbol) ||| (Parse.char '`' |>| word |>| Parse.char '`')
    let string = Parse.char '"' |>| Parse.noneOrMore (Parse.string "\\\"" ||| Parse.noneOf "\"") |>| Parse.char '"'
    let char = Parse.char '\'' |>| Parse.noneOrMore (Parse.string "\\'" ||| Parse.noneOf "'") |>| Parse.char '\''
    let whitespace = Parse.oneOrMore Parse.whitespace
    
    let keywordsList = 
        "as case of class data data family data instance default deriving instance do forall foreign hiding if then else import infix infixl, infixr instance let in mdo module newtype proc qualified rec type type family type instance where"
            .Split(' ')
        |> List.ofSeq
        |> List.map List.ofSeq
    
    let keyword chars = 
        match word chars with
        | Some(left, _) as x when List.contains left keywordsList -> x
        | _ -> None
    
    let tagger name parser = StateParse.withStateFunc (Func.constFunc name) parser ""
    
    let parsers = 
        [ tagger "newline" newline
          tagger "comment" comment
          tagger "comment" multilineComment
          tagger "number" number
          tagger "keyword" keyword
          tagger "identifier" word
          tagger "operator" operator
          tagger "string" string
          tagger "char" char
          tagger "whitespace" whitespace
          tagger "unrecognized" Parse.any ]
    
    let codeParser = List.reduce (|||) parsers
    
    let takeToken tokens chars = 
        match codeParser chars with
        | Some(part, rest, name) -> ((part, name) :: tokens, rest)
        | None -> (tokens, [])
    
    let (|NewLine|_|) = newline
    let (|Indentation|_|) = Parse.whitespace
    
    let rec advanceLineColumn (line, column) token = 
        match token with
        | [] -> (line, column)
        | _ :: xs -> 
            match token with
            | NewLine(_, rest) -> advanceLineColumn (line + 1, 0) rest
            | Indentation(c, rest) when List.head c = '\t' -> advanceLineColumn (line, column + TabLength) rest
            | _ -> advanceLineColumn (line, column + 1) xs
    
    let rec initial list = 
        match list with
        | x :: [] -> []
        | [] -> failwith "Empty list"
        | x :: xs -> x :: initial xs
    
    let toTokens chars = 
        let tokens = List.splitFold takeToken [] chars |> List.rev
        let locations = List.scan advanceLineColumn (0, 0) (List.map fst tokens) |> initial
        
        let combiner = 
            Func.curry3 Token
            |> Func.flip
            |> Func.uncurry
        List.map2 combiner tokens locations
    
    let line tokens = 
        let rec takeLineAcc tokens acc = 
            match tokens with
            | Token("newline", _, _) :: xs | ([] as xs) -> (List.rev acc, xs)
            | x :: xs -> takeLineAcc xs (x :: acc)
        match tokens with
        | [] -> None
        | _ -> Some <| takeLineAcc tokens []
    
    let takeLine lines tokens = 
        match line tokens with
        | Some(line, rest) -> (line :: lines, rest)
        | None -> (lines, [])
    
    let toLines tokens = List.splitFold takeLine [] tokens |> List.rev
