namespace Lox

type Value =
    | String of string
    | Number of double
    | Boolean of bool
    | Unit

[<RequireQualifiedAccess>]
module Runner =

    let fetchBlock (tokens: Token list) =

        let rec inner tokens cur bracketCount =

            match tokens with
            | [] -> (None, [])
            | OpenBracket :: tail -> inner tail cur (bracketCount + 1)
            | CloseBracket :: tail when bracketCount = 1 -> (cur |> List.rev |> Some, tail)
            | token :: tail -> inner tail (token :: cur) bracketCount

        inner tokens [] 0

    let fetchListOfArgs (tokens: Token list) : string list * Token list =

        let rec inner (tokens: Token list) (cur: string list) =
            match tokens with
            | [] -> ([], [])
            | CloseParenthesis :: tail -> (cur |> List.rev, tail)
            | Comma :: tail -> inner tail cur
            | Identifier iden :: tail -> inner tail (iden :: cur)
            | a :: _ -> failwith $"Unexpected token: {a}"

        match tokens with
        | OpenParenthesis :: tail -> inner tail []
        | _ -> failwith "unexpected token list"

    let fetchExpressionArgs (tokens: Token list) : Token list * Token list =
        let rec inner (tokens: Token list) (cur: Token list) =
            match tokens with
            | Semicolon :: tail -> cur |> List.rev, tail
            | h :: tail -> inner tail (h :: cur)
            | [] -> failwith $"Expected to find a semicolon"

        inner tokens []


    let valueToString (value: Value) : string =
        match value with
        | String s -> s
        | Number n -> n |> string
        | Boolean b -> b |> string
        | Unit -> "()"

    let addValues (l: Value) (r: Value) : Value =
        match l, r with
        | String l, String r -> String(l + r)
        | Number l, Number r -> Number(l + r)
        | _ -> failwith $"Can't add these values as not the right types: {l}, {r}"

    let tokenToValue (tok: Token) (state: Map<string, Value>) : Value =
        match tok with
        | Identifier name ->
            Map.tryFind name state
            |> Option.defaultWith (fun () -> failwith $"Failed to find {name} in {state}")
        | Token.String str -> String str
        | Token.Boolean bl -> Boolean bl
        | _ -> failwith $"Token does not represent a value: {tok}"


    let rec evalExpression (tokens: Token list) (state: Map<string, Value>) : Value =
        match tokens with
        | Identifier name :: [] -> Map.find name state
        | token :: [] -> tokenToValue token state
        | token :: Plus :: tail ->
            let rValue = evalExpression tail state
            let lValue = tokenToValue token state
            addValues lValue rValue
        | _ -> failwith $"Unsupported expression: {tokens}"

    type FunctionCallArgValue =
        | Variable of string
        | Value of Value

    let fetchFunctionCallArgs (tokens: Token list) (state: Map<string, Value>) : Value list * Token list =

        let rec inner (tail: Token list) (cur: Value list) =
            match tail with
            | CloseParenthesis :: Semicolon :: tail -> cur |> List.rev, tail
            | Comma :: tail -> inner tail cur
            | token :: tail -> inner tail (tokenToValue token state :: cur)
            | [] -> failwith $"Expected to find a semicolon"

        inner tokens []


    let run (print: string -> unit) (tokens: Token list) : Map<string, Value> =
        let rec inner tokens state functions : Map<string, Value> * Value =
            match tokens with
            | [] -> state, Unit
            | Var :: Identifier name :: Equals :: tail ->
                let state, tail =
                    match tail with
                    | Identifier functionName :: OpenParenthesis :: tail ->
                        match Map.tryFind functionName functions with
                        | Some(args, block) ->

                            let callArgs, tail = fetchFunctionCallArgs tail state

                            let state = callArgs |> Seq.zip args |> Map.ofSeq

                            let _, ret = inner block state functions

                            Map.add name ret state, tail
                        | None -> failwith ("Undefined function " + functionName)
                    | _ ->
                        let expressionTokens, tail = fetchExpressionArgs tail

                        let value = evalExpression expressionTokens state

                        let state = Map.add name value state
                        state, tail

                inner tail state functions

            | Print :: OpenParenthesis :: value :: CloseParenthesis :: Semicolon :: tail ->
                match value with
                | Token.String str -> printfn "%A" str
                | Identifier id ->
                    match Map.tryFind id state with
                    | Some value -> value |> valueToString |> print
                    | None -> failwith ("unknown variable " + id)
                | _ -> failwith ("unsupported arg to print: " + (sprintf "%A" value))

                inner tail state functions

            | Fun :: Identifier functionName :: tail ->
                let (newFunctions, newTail) =
                    let args, tail = fetchListOfArgs tail

                    match fetchBlock tail with
                    | (None, _) -> failwith $"Got invalid block, tokens: {tail}"
                    | (Some block, tail) -> (Map.add functionName (args, block) functions, tail)

                inner newTail state newFunctions

            | Return :: Identifier f :: Semicolon :: _ -> state, (state |> Map.find f)

            | tokens -> failwith $"invalid expression: {tokens}. State: {state}"

        inner tokens Map.empty Map.empty |> fst
