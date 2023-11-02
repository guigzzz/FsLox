namespace Lox

[<RequireQualifiedAccess>]
module Runner =

    let fetchMatchingBracket (tokens: Token list) (openBracket: Token) (closeBracket: Token) : Token list * Token list =
        let rec inner tokens cur bracketCount =

            match tokens with
            | [] -> failwith $"Expected some more tokens, cur={cur}, bracketCount={bracketCount}"
            | b :: tail when b = openBracket -> inner tail (b :: cur) (bracketCount + 1)
            | b :: tail when b = closeBracket && bracketCount = 1 -> (cur |> List.rev, tail)
            | b :: tail when b = closeBracket -> inner tail (b :: cur) (bracketCount - 1)
            | token :: tail -> inner tail (token :: cur) bracketCount

        inner tokens [] 1

    let fetchMatchingBracketExpectOpen
        (tokens: Token list)
        (openBracket: Token)
        (closeBracket: Token)
        : Token list * Token list =
        match tokens with
        | b :: tail when b = openBracket -> fetchMatchingBracket tail openBracket closeBracket
        | tail -> failwith $"Expected {openBracket} but got: {tail}"

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
            | [] -> failwith $"Expected to find a {stopToken}"

        inner tokens []

    let fetchFunctionCallArgs (tokens: Token list) (c: Context) : Value list * Token list =

        let rec inner (tail: Token list) (cur: Value list) =
            match tail with
            | CloseParenthesis :: Semicolon :: tail -> cur |> List.rev, tail
            | Comma :: tail -> inner tail cur
            | token :: tail -> inner tail (ValueContext.ofToken token c :: cur)
            | [] -> failwith $"Expected to find a semicolon"

        inner tokens []

    let splitByTokenPreserveParens (token: Token) (toks: Token list) : Token list list =
        let rec inner (toks: Token list) (cur: Token list) (out: Token list list) (curBracket: int) =
            match toks with
            | [] -> ((cur |> List.rev) :: out) |> List.rev
            | t :: tail when t = token && curBracket = 0 ->
                let cur = cur |> List.rev
                inner tail [] (cur :: out) curBracket
            | t :: tail when t = OpenParenthesis -> inner tail (t :: cur) out (curBracket + 1)
            | t :: tail when t = CloseParenthesis -> inner tail (t :: cur) out (curBracket - 1)
            | t :: tail -> inner tail (t :: cur) out curBracket

        match toks with
        | [] -> []
        | toks -> inner toks [] [] 0

    let callFunc (callArgs: Value list) (name: string) (context: Context) : Value =
        let func = context |> Context.getFunc name
        let localFunctionState = callArgs |> Seq.zip func.Args |> Map.ofSeq

        let allState = context.Variables |> Map.merge localFunctionState

        func.Func allState

    let rec evalExpression (context: Context) (tokens: Token list) : Value =
        match tokens with
        | Identifier name :: [] -> context |> Context.getVar name
        | token :: [] -> ValueContext.ofToken token context
        | token :: Plus :: tail ->
            let rValue = evalExpression context tail
            let lValue = ValueContext.ofToken token context
            Value.add lValue rValue
        | Identifier name :: OpenParenthesis :: tail ->
            let args, tail = fetchMatchingBracket tail OpenParenthesis CloseParenthesis

            let callArgs =
                args |> splitByTokenPreserveParens Comma |> List.map (evalExpression context)

            if tail |> List.isEmpty |> not then
                failwith $"operations after a function call isn't yet supported. Toks: {tail}"

            context |> callFunc callArgs name

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

            let trueBlock, tail = fetchMatchingBracket tail OpenBracket CloseBracket

            let falseBlock, tail =
                match tail with
                | Else :: tail ->
                    let block, tail = fetchMatchingBracketExpectOpen tail OpenBracket CloseBracket
                    block |> Some, tail
                | _ -> None, tail

            let block = if b then trueBlock |> Some else falseBlock

            block, tail

    let run (print: string -> unit) (tokens: Token list) : Variables =
        let rec inner tokens (context: Context) : Context * Value =

            let varAssignment name tail =
                match tail with
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


            match tokens with
            | [] -> context, Unit
            | Var :: Identifier name :: Equals :: tail ->
                let context, tail = varAssignment name tail
                inner tail context

            | Identifier name :: BackArrow :: tail ->
                if context |> Context.varExists name |> not then
                    failwith $"Can't reassign a variable that wasn't declared: '{name}'"

                let context, tail = varAssignment name tail
                inner tail context

            | For :: Identifier loopVar :: In :: startToken :: DoubleDot :: endToken :: OpenBracket :: tail ->

                let block, tail = fetchMatchingBracket tail OpenBracket CloseBracket

                let s = startToken |> ValueContext.toNumber context
                let e = endToken |> ValueContext.toNumber context

                let retContext =
                    [ s .. (e - 1.) ]
                    |> List.fold
                        (fun context i -> context |> Context.addVar loopVar (Number i) |> inner block |> fst)
                        context

                let newContext =
                    context.Variables
                    |> Map.keys
                    |> Seq.map (fun k -> k, Map.find k retContext.Variables)
                    |> Map.ofSeq
                    |> fun vars -> { context with Variables = vars }

                inner tail newContext

            | If :: tail ->
                let tokens, tail = fetchIfTokens tail context

                match tokens with
                | None -> ()
                | Some tokens -> inner tokens context |> ignore

                inner tail context

            | Fun :: Identifier functionName :: tail ->
                let (newContext, newTail) =
                    let args, tail = fetchListOfArgs tail

                    let block, tail = fetchMatchingBracketExpectOpen tail OpenBracket CloseBracket

                    let runFunc vars =
                        inner block ({ context with Variables = vars }) |> snd

                    let func = Function.make args runFunc

                    context |> Context.addFunc functionName func, tail

                inner newTail newContext

            | (Identifier _ :: OpenParenthesis :: _) as tail ->

                let expressionTokens, tail = fetchExpressionArgs tail Semicolon

                let _ = evalExpression context expressionTokens

                inner tail context

            | Return :: tail ->
                let expressionTokens, tail = fetchExpressionArgs tail Semicolon
                let value = evalExpression context expressionTokens
                context, value

            | tokens -> failwith $"invalid expression: {tokens}. Context: {context}"

        print |> Context.make |> inner tokens |> fst |> (fun c -> c.Variables)
