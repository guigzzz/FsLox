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

    let toBoolean (value: Value) : bool option =
        match value with
        | Boolean b -> b |> Some
        | _ -> None

type Variables = Map<string, Value>

type Function =
    { Args: string list
      Func: Variables -> Value }

[<RequireQualifiedAccess>]
module Function =
    let make args func : Function = { Args = args; Func = func }

type Context =
    { Variables: Variables
      Functions: Map<string, Function> }

[<RequireQualifiedAccess>]
module Context =
    let make (print: string -> unit) =
        let printBuiltinArg = ""

        let print vars =
            vars |> Map.find printBuiltinArg |> Value.toString |> print
            Unit

        { Variables = Map.empty
          Functions = [ "print", Function.make [ printBuiltinArg ] print ] |> Map.ofSeq }

    let getFunc (name: string) (c: Context) : Function =
        c.Functions
        |> Map.tryFind name
        |> Option.defaultWith (fun () ->
            let funs = c.Functions |> Map.keys |> String.concat ", "
            failwith $"Failed to find function called {name}. Options were: {funs}")

    let addFunc (name: string) (func: Function) (c: Context) : Context =
        { c with
            Functions = c.Functions |> Map.add name func }

    let addVar (name: string) (value: Value) (c: Context) : Context =
        { c with
            Variables = c.Variables |> Map.add name value }

    let getVar (name: string) (c: Context) : Value =
        c.Variables
        |> Map.tryFind name
        |> Option.defaultWith (fun () ->
            let funs = c.Variables |> Map.keys |> String.concat ", "
            failwith $"Failed to find variable called '{name}'. Options were: {funs}")

    let varExists (name: string) (c: Context) : bool = c.Variables |> Map.containsKey name

[<RequireQualifiedAccess>]
module ValueContext =

    let ofToken (tok: Token) (c: Context) : Value =
        match tok with
        | Identifier name -> c |> Context.getVar name
        | Token.String str -> String str
        | Token.Boolean bl -> Boolean bl
        | Token.Number n -> Number n
        | _ -> failwith $"Token does not represent a value: {tok}"
