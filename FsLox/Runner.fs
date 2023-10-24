namespace Lox



[<RequireQualifiedAccess>]
module Runner =

    let fetchMatchingBracket (tokens: Token list) (openBracket: Token) (closeBracket: Token) : Token list * Token list =
        let rec inner tokens cur bracketCount =

            match tokens with
            | [] -> failwith $"Expected some more tokens"
            | b :: tail when b = openBracket -> inner tail cur (bracketCount + 1)
            | b :: tail when b = closeBracket && bracketCount = 1 -> (cur |> List.rev, tail)
            | token :: tail -> inner tail (token :: cur) bracketCount

        inner tokens [] 1

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


    let fetchFunctionCallArgs (tokens: Token list) (c: Context) : Value list * Token list =

        let rec inner (tail: Token list) (cur: Value list) =
            match tail with
            | CloseParenthesis :: Semicolon :: tail -> cur |> List.rev, tail
            | Comma :: tail -> inner tail cur
            | token :: tail -> inner tail (Value.ofToken token c :: cur)
            | [] -> failwith $"Expected to find a semicolon"

        inner tokens []

    let splitByToken (token: Token) (toks: Token list) : Token list list =
        let rec inner (toks: Token list) (cur: Token list) (out: Token list list) =
            match toks with
            | [] -> out
            | t :: tail when t = token ->
                let cur = cur |> List.rev
                inner tail [] (cur :: out)
            | t :: tail -> inner tail (t :: cur) out

        inner toks [] []

    let rec evalExpression (context: Context) (tokens: Token list) : Value =
        match tokens with
        | Identifier name :: [] -> context |> Context.getVar name
        | token :: [] -> Value.ofToken token context
        | token :: Plus :: tail ->
            let rValue = evalExpression context tail
            let lValue = Value.ofToken token context
            Value.add lValue rValue
        | Identifier name :: OpenParenthesis :: tail ->

            let args, tail = fetchMatchingBracket tail OpenParenthesis CloseParenthesis

            let callArgs = args |> splitByToken Comma |> List.map (evalExpression context)

            let func = context |> Context.getFunc name

            let localFunctionState = callArgs |> Seq.zip func.Args |> Map.ofSeq

            if tail |> List.isEmpty |> not then
                failwith $"operations after a function call isn't yet supported"

            func.Func localFunctionState
        | _ -> failwith $"Unsupported expression: {tokens}"

    type FunctionCallArgValue =
        | Variable of string
        | Value of Value



    let fetchIfTokens (tokens: Token list) (context: Context) : Token list option * Token list =

        let expressionTokens, tail = fetchExpressionArgs tokens OpenBracket

        let value = evalExpression context expressionTokens

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

    let run (print: string -> unit) (tokens: Token list) : Variables =
        let rec inner tokens (context: Context) : Variables * Value =
            let functions = context.Functions
            let state = context.Variables

            match tokens with
            | [] -> context.Variables, Unit
            | Var :: Identifier name :: Equals :: tail ->
                let newContext, tail =
                    match tail with
                    | Identifier functionName :: OpenParenthesis :: tail ->
                        match Map.tryFind functionName functions with
                        | Some f ->

                            let callArgs, tail = fetchFunctionCallArgs tail context

                            let localFunctionState = callArgs |> Seq.zip f.Args |> Map.ofSeq

                            let value = f.Func localFunctionState

                            context |> Context.addVar name value, tail
                        | None -> failwith ("Undefined function " + functionName)
                    | If :: tail ->
                        let tokens, tail = fetchIfTokens tail context

                        let tokens =
                            tokens
                            |> Option.defaultWith (fun () ->
                                failwith
                                    $"When assigning the result of an if-else to a variable, you need to provide both cases")

                        let _, value = inner tokens context
                        context |> Context.addVar name value, tail
                    | _ ->
                        let expressionTokens, tail = fetchExpressionArgs tail Semicolon

                        let value = evalExpression context expressionTokens

                        context |> Context.addVar name value, tail

                inner tail newContext

            | If :: tail ->
                let tokens, tail = fetchIfTokens tail context

                match tokens with
                | None -> ()
                | Some tokens -> inner tokens context |> ignore

                inner tail context

            | Print :: OpenParenthesis :: value :: CloseParenthesis :: Semicolon :: tail ->
                match value with
                | Token.String str -> str |> print
                | Identifier id ->
                    match Map.tryFind id state with
                    | Some value -> value |> Value.toString |> print
                    | None -> failwith ("unknown variable " + id)
                | _ -> failwith ("unsupported arg to print: " + (sprintf "%A" value))

                inner tail context

            | Fun :: Identifier functionName :: tail ->
                let (newContext, newTail) =
                    let args, tail = fetchListOfArgs tail


                    let block, tail = fetchBlock tail

                    let runFunc vars =
                        inner block ({ context with Variables = vars }) |> snd

                    let func = Function.make args runFunc

                    context |> Context.addFunc functionName func, tail

                inner newTail newContext

            | Return :: tail ->
                let expressionTokens, tail = fetchExpressionArgs tail Semicolon
                let value = evalExpression context expressionTokens
                state, value

            | tokens -> failwith $"invalid expression: {tokens}. State: {state}"

        inner tokens Context.empty |> fst
