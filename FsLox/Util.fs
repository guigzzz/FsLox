namespace Lox

open System.Collections.Generic

[<RequireQualifiedAccess>]
module Dictionary =
    let ofMap (map: Map<_, _>) : IDictionary<_, _> = map |> Map.toSeq |> dict
