namespace Lox

type Token =
    | Var
    | Identifier of string
    | Equals
    | String of string
    | Boolean of bool
    | Print
    | Semicolon
    | Fun
    | OpenParenthesis
    | CloseParenthesis
    | OpenBracket
    | CloseBracket
    | Plus
    | Comma
    | Return

[<RequireQualifiedAccess>]
module Tokeniser =

    let stringOfChars chars = new string [| for c in chars -> c |]

    let fetchString chars =
        let rec inner chars cur =
            match chars with
            | [] -> (None, [])
            | '"' :: tail -> (cur |> List.rev |> stringOfChars |> Some, tail)
            | c :: tail -> inner tail (c :: cur)

        inner chars []

    let fetchIdentifier chars =
        let rec inner chars cur =
            match chars with
            | []
            | ' ' :: _
            | '=' :: _
            | ';' :: _
            | '(' :: _
            | ')' :: _ -> (cur |> List.rev |> stringOfChars |> Some, chars)
            | c :: tail -> inner tail (c :: cur)

        inner chars []

    let rec tokenize chars : list<Token> =
        match chars with
        | [] -> []
        | ' ' :: tail
        | '\n' :: tail -> tokenize tail
        | ';' :: tail -> Semicolon :: tokenize tail
        | '=' :: tail -> Equals :: tokenize tail
        | 'v' :: 'a' :: 'r' :: tail -> Var :: tokenize tail
        | 'p' :: 'r' :: 'i' :: 'n' :: 't' :: tail -> Print :: tokenize tail
        | 'f' :: 'u' :: 'n' :: tail -> Fun :: tokenize tail
        | '(' :: tail -> OpenParenthesis :: tokenize tail
        | ')' :: tail -> CloseParenthesis :: tokenize tail
        | '{' :: tail -> OpenBracket :: tokenize tail
        | '}' :: tail -> CloseBracket :: tokenize tail
        | ',' :: tail -> Comma :: tokenize tail
        | '+' :: tail -> Plus :: tokenize tail
        | 'r' :: 'e' :: 't' :: 'u' :: 'r' :: 'n' :: tail -> Return :: tokenize tail
        | '"' :: tail ->
            match fetchString tail with
            | (None, _) -> failwith "unbounded string"
            | (Some str, tail) -> String str :: tokenize tail
        | tail ->
            match fetchIdentifier tail with
            | (None, _) -> failwith ("unhandled case: " + stringOfChars chars)
            | (Some str, tail) -> Identifier str :: tokenize tail
