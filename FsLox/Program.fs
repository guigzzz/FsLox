open Lox

[<EntryPoint>]

let main argv =
    let tokens =
        argv
        |> Array.head
        |> System.IO.File.ReadAllText
        |> Seq.toList
        |> Tokeniser.tokenise

    tokens |> printfn "%A"

    printfn "--- Interpreter stdout --- "

    let state = tokens |> Runner.run (printfn "%s")

    printfn "--- Final state ---"

    printfn "%A" state

    0 // return an integer exit code
