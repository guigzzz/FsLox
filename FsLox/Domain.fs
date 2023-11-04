namespace Lox

type Variables = Map<string, Value>

and Object =
    { Type: string
      Variables: Variables
      Functions: Map<string, Function> }

and Function =
    { Name: string
      Args: string list
      Func: StructurallyNull<Variables -> Value> }

and ArrayList = { Values: Value[] }

and Value =
    | String of string
    | Number of double
    | Boolean of bool
    | Object of Object
    | ArrayList of ArrayList
    | Unit

[<RequireQualifiedAccess>]
module Value =

    let rec toString (value: Value) : string =
        match value with
        | String s -> s
        | Number n -> n |> string
        | Boolean b -> b |> string
        | Object o ->
            let vars =
                o.Variables
                |> Map.toSeq
                |> Seq.map (fun (k, v) -> $"{k}: {toString v}")
                |> String.concat ", "

            $"{o.Type}({vars})"
        | ArrayList a ->
            let vars = a.Values |> Seq.map toString |> String.concat ", "
            $"[{vars}]"
        | Unit -> "()"

    let add (l: Value) (r: Value) : Value =
        match l, r with
        | String l, String r -> String(l + r)
        | Number l, Number r -> Number(l + r)
        | _ -> failwith $"Can't add these values as not the right types: {l}, {r}"

    let subtract (l: Value) (r: Value) : Value =
        match l, r with
        | Number l, Number r -> Number(l - r)
        | _ -> failwith $"Can't subtract these values as not the right types: {l}, {r}"

    let multiply (l: Value) (r: Value) : Value =
        match l, r with
        | Number l, Number r -> Number(l * r)
        | _ -> failwith $"Can't multiply these values as not the right types: {l}, {r}"

    let divide (l: Value) (r: Value) : Value =
        match l, r with
        | Number l, Number r -> Number(l / r)
        | _ -> failwith $"Can't divide these values as not the right types: {l}, {r}"

    let toBoolean (value: Value) : bool option =
        match value with
        | Boolean b -> b |> Some
        | _ -> None

    let toNumber (value: Value) : double option =
        match value with
        | Number b -> b |> Some
        | _ -> None

    let toObject (value: Value) : Object option =
        match value with
        | Object o -> o |> Some
        | _ -> None

    let toArrayList (value: Value) : ArrayList option =
        match value with
        | ArrayList a -> a |> Some
        | _ -> None


[<RequireQualifiedAccess>]
module Function =
    let make name args func : Function =
        { Name = name
          Args = args
          Func = StructurallyNull.make func }

    let call state (f: Function) = f.Func.V state

[<RequireQualifiedAccess>]
module Object =

    let makeInstantiator (typ: string) (funcs: Map<string, Function>) : Function =
        let defaultConstructor () =
            let object =
                { Type = typ
                  Variables = Map.empty
                  Functions = funcs }

            Function.make "constructor" List.empty (fun _ -> object |> Object)

        funcs |> Map.tryFind "constructor" |> Option.defaultWith defaultConstructor

    let getFunc (name: string) (object: Object) : Function =
        object.Functions
        |> Map.tryFind name
        |> Option.defaultWith (fun () -> failwith $"Can't call {name} on object of type {object.Type}")


type Context =
    { Variables: Variables
      Functions: Map<string, Function> }

[<RequireQualifiedAccess>]
module ListObject =

    let make (initValues: Value list) =
        let rec append vars =
            let value =
                vars
                |> Map.tryFind "internal_array"
                |> Option.bind Value.toArrayList
                |> Option.defaultWith (fun () -> failwith "Failed to fetch List array")

            let valueToAdd = vars |> Map.find "value"

            let newArray = valueToAdd |> Array.singleton |> Array.append value.Values

            { Type = "List"
              Variables = [ "internal_array", { Values = newArray } |> ArrayList ] |> Map.ofSeq
              Functions = [ "append", Function.make "append" [ "value" ] append ] |> Map.ofSeq }
            |> Object

        { Type = "List"
          Variables =
            [ "internal_array", { Values = initValues |> Array.ofList } |> ArrayList ]
            |> Map.ofSeq

          Functions = [ "append", Function.make "append" [ "value" ] append ] |> Map.ofSeq }
        |> Object

    let empty = List.empty |> make


[<RequireQualifiedAccess>]
module Context =
    let make (print: string -> unit) =
        let printBuiltinArg = ""

        let print vars =
            vars |> Map.find printBuiltinArg |> Value.toString |> print
            Unit

        { Variables = Map.empty
          Functions =
            [ "print", Function.make "print" [ printBuiltinArg ] print
              "list", Function.make "list" [] (fun _ -> ListObject.empty) ]
            |> Map.ofSeq }

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

    let removeVar (name: string) (c: Context) : Context =
        { c with
            Variables = c.Variables |> Map.remove name }

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

    let toNumber (c: Context) (tok: Token) : double =
        match tok with
        | Identifier name -> c |> Context.getVar name |> Value.toNumber
        | Token.Number n -> n |> Some
        | _ -> failwith $"Token does not represent a value: {tok}"
        |> Option.defaultWith (fun () -> failwith $"{tok} did not represent a numeric value")


type Operator =
    | Add
    | Multiply
    | Divide
    | Subtract

[<RequireQualifiedAccess>]
module Operator =

    let ofToken (token: Token) : Operator option =
        match token with
        | Plus -> Add |> Some
        | Token.Subtract -> Subtract |> Some
        | Token.Divide -> Divide |> Some
        | Token.Multiply -> Multiply |> Some
        | _ -> None

    let apply (operator: Operator) (left: Value) (right: Value) : Value =
        match operator with
        | Add -> Value.add left right
        | Subtract -> Value.subtract left right
        | Divide -> Value.divide left right
        | Multiply -> Value.multiply left right
