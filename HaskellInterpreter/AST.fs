namespace Haskell

open FSharpExt

module AST = 
    let toIndentLinePair tokens = 
        match tokens with
        | Token("whitespace", ws, _) :: xs -> 
            let level = 
                List.map (fun c -> 
                    if c = '\t' then 4
                    else 1) ws
                |> List.sum
            if level % 4 <> 0 then 
                failwith 
                <| System.String.Format
                       ("Indentation is expected to be a multiple of {0}. Tab character is considered {0} spaces.", 
                        TabLength)
            (level / 4, xs)
        | x -> (0, x)
    
    let removeEmptyLines lines = 
        List.filter (fun line -> 
            match line with
            | [] | Token("whitespace", _, _) :: [] -> false
            | _ -> true) lines

    let rec toExpression lines =
        match lines with
        | (_, statement) :: [] -> Statement(statement)
        | (initialLevel, statement) :: xs -> 
            let groups = List.splitBefore (fst >> (=) (initialLevel + 1)) xs
            Expression(statement, List.map toExpression groups)
        | [] -> failwith "No lines given"

    let toIndentList lines = List.splitBefore (fst >> (=) 0) lines |> List.map toExpression |> IndentList

    let toIndentTree tokens = Tokenize.toLines tokens |> removeEmptyLines |> List.map toIndentLinePair |> toIndentList

    let fromTokens (tokens : Token list) : AST = failwith "Not impletemented"

