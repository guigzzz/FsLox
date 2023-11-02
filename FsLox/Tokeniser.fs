namespace Lox

open System


type Token =
    | Var
    | Identifier of string
    | Equals
    | String of string
    | Number of double
    | Boolean of bool
    | Semicolon
    | Fun
    | OpenParenthesis
    | CloseParenthesis
    | OpenBracket
    | CloseBracket
    | Plus
    | Subtract
    | Multiply
    | Divide
    | For
    | In
    | DoubleDot
    | Comma
    | Return
    | If
    | Else
    | BackArrow

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
            | [] -> (None, [])
            | c :: tail when c |> Char.IsLetter || c |> Char.IsDigit || c = '_' -> inner tail (c :: cur)
            | _ -> (cur |> List.rev |> stringOfChars |> Some, chars)

        inner chars []

    let private fetchNumber chars : double option * char list =
        let rec inner chars cur =
            match chars with
            | [] -> (None, [])
            | c :: tail when c |> Char.IsDigit -> inner tail (c :: cur)
            | _ -> (cur |> List.rev |> stringOfChars |> Double.Parse |> Some, chars)

        inner chars []

    let notIdentifierCharacter (chars: char list) : bool =
        match chars |> List.tryHead with
        | Some c -> c |> Char.IsLetter || c |> Char.IsDigit || c = '_'
        | _ -> false
        |> not


    let rec tokenise chars : list<Token> =
        match chars with
        | [] -> []
        | ' ' :: tail
        | '\n' :: tail
        | '\010' :: tail
        | '\013' :: tail -> tokenise tail
        | ';' :: tail -> Semicolon :: tokenise tail
        | '=' :: tail -> Equals :: tokenise tail
        | 'v' :: 'a' :: 'r' :: tail when notIdentifierCharacter tail -> Var :: tokenise tail
        | 'f' :: 'u' :: 'n' :: tail when notIdentifierCharacter tail -> Fun :: tokenise tail
        | 't' :: 'r' :: 'u' :: 'e' :: tail when notIdentifierCharacter tail -> Boolean true :: tokenise tail
        | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: tail when notIdentifierCharacter tail -> Boolean false :: tokenise tail
        | 'i' :: 'f' :: tail when notIdentifierCharacter tail -> If :: tokenise tail
        | 'e' :: 'l' :: 's' :: 'e' :: tail when notIdentifierCharacter tail -> Else :: tokenise tail
        | '<' :: '-' :: tail -> BackArrow :: tokenise tail
        | 'f' :: 'o' :: 'r' :: tail when notIdentifierCharacter tail -> For :: tokenise tail
        | 'i' :: 'n' :: tail when notIdentifierCharacter tail -> In :: tokenise tail
        | '.' :: '.' :: tail -> DoubleDot :: tokenise tail
        | '(' :: tail -> OpenParenthesis :: tokenise tail
        | ')' :: tail -> CloseParenthesis :: tokenise tail
        | '{' :: tail -> OpenBracket :: tokenise tail
        | '}' :: tail -> CloseBracket :: tokenise tail
        | ',' :: tail -> Comma :: tokenise tail
        | '+' :: tail -> Plus :: tokenise tail
        | '-' :: tail -> Subtract :: tokenise tail
        | '*' :: tail -> Multiply :: tokenise tail
        | '/' :: tail -> Divide :: tokenise tail
        | 'r' :: 'e' :: 't' :: 'u' :: 'r' :: 'n' :: tail when notIdentifierCharacter tail -> Return :: tokenise tail
        | '"' :: tail ->
            match fetchString tail with
            | (None, _) -> failwith "unbounded string"
            | (Some str, tail) -> String str :: tokenise tail
        | tail when tail |> List.head |> Char.IsLetter ->
            match fetchIdentifier tail with
            | (None, _) -> failwith $"unhandled case: {stringOfChars chars}"
            | (Some str, tail) -> Identifier str :: tokenise tail

        | tail when tail |> List.head |> Char.IsDigit ->
            match fetchNumber tail with
            | (None, _) -> failwith $"failed to parse number from chars: {stringOfChars chars}"
            | (Some v, tail) -> Number v :: tokenise tail
        | _ -> failwith $"Unexpected chars: {stringOfChars chars}"

    let tokenise': string -> Token list = Seq.toList >> tokenise
