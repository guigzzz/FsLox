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
    | If
    | Else

[<RequireQualifiedAccess>]
module Tokeniser =

    let private stringOfChars chars = new string [| for c in chars -> c |]

    let private fetchString chars =
        let rec inner chars cur =
            match chars with
            | [] -> (None, [])
            | '"' :: tail -> (cur |> List.rev |> stringOfChars |> Some, tail)
            | c :: tail -> inner tail (c :: cur)

        inner chars []

    let private fetchIdentifier chars =
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

    let rec tokenise chars : list<Token> =
        match chars with
        | [] -> []
        | ' ' :: tail
        | '\n' :: tail
        | '\010' :: tail
        | '\013' :: tail -> tokenise tail
        | ';' :: tail -> Semicolon :: tokenise tail
        | '=' :: tail -> Equals :: tokenise tail
        | 'v' :: 'a' :: 'r' :: tail -> Var :: tokenise tail
        | 'p' :: 'r' :: 'i' :: 'n' :: 't' :: tail -> Print :: tokenise tail
        | 'f' :: 'u' :: 'n' :: tail -> Fun :: tokenise tail
        | 't' :: 'r' :: 'u' :: 'e' :: tail -> Boolean true :: tokenise tail
        | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: tail -> Boolean false :: tokenise tail
        | 'i' :: 'f' :: tail -> If :: tokenise tail
        | 'e' :: 'l' :: 's' :: 'e' :: tail -> If :: tokenise tail
        | '(' :: tail -> OpenParenthesis :: tokenise tail
        | ')' :: tail -> CloseParenthesis :: tokenise tail
        | '{' :: tail -> OpenBracket :: tokenise tail
        | '}' :: tail -> CloseBracket :: tokenise tail
        | ',' :: tail -> Comma :: tokenise tail
        | '+' :: tail -> Plus :: tokenise tail
        | 'r' :: 'e' :: 't' :: 'u' :: 'r' :: 'n' :: tail -> Return :: tokenise tail
        | '"' :: tail ->
            match fetchString tail with
            | (None, _) -> failwith "unbounded string"
            | (Some str, tail) -> String str :: tokenise tail
        | tail ->
            match fetchIdentifier tail with
            | (None, _) -> failwith ("unhandled case: " + stringOfChars chars)
            | (Some str, tail) -> Identifier str :: tokenise tail

    let tokenise': string -> Token list = Seq.toList >> tokenise
