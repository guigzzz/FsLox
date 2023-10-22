namespace Lox

type Value =
    | String of string
    | Number of double
    | Boolean of bool
    | Unit

[<RequireQualifiedAccess>]
module Value =

    let toString (value: Value) : string =
        match value with
        | String s -> s
        | Number n -> n |> string
        | Boolean b -> b |> string
        | Unit -> "()"

    let add (l: Value) (r: Value) : Value =
        match l, r with
        | String l, String r -> String(l + r)
        | Number l, Number r -> Number(l + r)
        | _ -> failwith $"Can't add these values as not the right types: {l}, {r}"

    let ofToken (tok: Token) (state: Map<string, Value>) : Value =
        match tok with
        | Identifier name ->
            Map.tryFind name state
            |> Option.defaultWith (fun () -> failwith $"Failed to find {name} in {state}")
        | Token.String str -> String str
        | Token.Boolean bl -> Boolean bl
        | Token.Number n -> Number n
        | _ -> failwith $"Token does not represent a value: {tok}"

    let toBoolean (value: Value) : bool option =
        match value with
        | Boolean b -> b |> Some
        | _ -> None

[<RequireQualifiedAccess>]
module Runner =

    let fetchBlock (tokens: Token list) : Token list * Token list =

        let rec inner tokens cur bracketCount =

            match tokens with
            | [] -> failwith $"Expected some more tokens"
            | OpenBracket :: tail -> inner tail cur (bracketCount + 1)
            | CloseBracket :: tail when bracketCount = 1 -> (cur |> List.rev, tail)
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

    let fetchExpressionArgs (tokens: Token list) (stopToken: Token) : Token list * Token list =
        let rec inner (tokens: Token list) (cur: Token list) =
            match tokens with
            | tok :: tail when tok = stopToken -> cur |> List.rev, tail
            | h :: tail -> inner tail (h :: cur)
            | [] -> failwith $"Expected to find a semicolon"

        inner tokens []

    let rec evalExpression (tokens: Token list) (state: Map<string, Value>) : Value =
        match tokens with
        | Identifier name :: [] -> Map.find name state
        | token :: [] -> Value.ofToken token state
        | token :: Plus :: tail ->
            let rValue = evalExpression tail state
            let lValue = Value.ofToken token state
            Value.add lValue rValue
        | _ -> failwith $"Unsupported expression: {tokens}"

    type FunctionCallArgValue =
        | Variable of string
        | Value of Value

    let fetchFunctionCallArgs (tokens: Token list) (state: Map<string, Value>) : Value list * Token list =

        let rec inner (tail: Token list) (cur: Value list) =
            match tail with
            | CloseParenthesis :: Semicolon :: tail -> cur |> List.rev, tail
            | Comma :: tail -> inner tail cur
            | token :: tail -> inner tail (Value.ofToken token state :: cur)
            | [] -> failwith $"Expected to find a semicolon"

        inner tokens []

    let fetchIfTokens (tokens: Token list) (state: Map<string, Value>) : Token list option * Token list =
        let expressionTokens, tail = fetchExpressionArgs tokens OpenBracket

        let value = evalExpression expressionTokens state

        match Value.toBoolean value with
        | None -> failwith $"expression {expressionTokens} did not evaluate to a bool"
        | Some b ->

            let trueBlock, tail = fetchBlock (OpenBracket :: tail)

            let falseBlock, tail =
                match tail with
                | Else :: tail ->
                    let block, tail = fetchBlock tail
                    block |> Some, tail
                | _ -> None, tail

            let block = if b then trueBlock |> Some else falseBlock

            block, tail

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

                            let localFunctionState = callArgs |> Seq.zip args |> Map.ofSeq

                            let _, ret = inner block localFunctionState functions

                            Map.add name ret state, tail
                        | None -> failwith ("Undefined function " + functionName)
                    | If :: tail ->
                        let tokens, tail = fetchIfTokens tail state

                        let tokens =
                            tokens
                            |> Option.defaultWith (fun () ->
                                failwith
                                    $"When assigning the result of an if-else to a variable, you need to provide both cases")

                        let _, ret = inner tokens state functions
                        Map.add name ret state, tail
                    | _ ->
                        let expressionTokens, tail = fetchExpressionArgs tail Semicolon

                        let value = evalExpression expressionTokens state

                        let state = Map.add name value state
                        state, tail

                inner tail state functions

            | If :: tail ->
                let tokens, tail = fetchIfTokens tail state

                match tokens with
                | None -> ()
                | Some tokens -> inner tokens state functions |> ignore

                inner tail state functions

            | Print :: OpenParenthesis :: value :: CloseParenthesis :: Semicolon :: tail ->
                match value with
                | Token.String str -> str |> print
                | Identifier id ->
                    match Map.tryFind id state with
                    | Some value -> value |> Value.toString |> print
                    | None -> failwith ("unknown variable " + id)
                | _ -> failwith ("unsupported arg to print: " + (sprintf "%A" value))

                inner tail state functions

            | Fun :: Identifier functionName :: tail ->
                let (newFunctions, newTail) =
                    let args, tail = fetchListOfArgs tail

                    let block, tail = fetchBlock tail
                    Map.add functionName (args, block) functions, tail

                inner newTail state newFunctions

            | Return :: tail ->
                let expressionTokens, tail = fetchExpressionArgs tail Semicolon
                let value = evalExpression expressionTokens state
                state, value

            | tokens -> failwith $"invalid expression: {tokens}. State: {state}"

        inner tokens Map.empty Map.empty |> fst
