namespace Lox

open System.Collections.Generic

[<RequireQualifiedAccess>]
module Dictionary =
    let ofMap (map: Map<_, _>) : IDictionary<_, _> = map |> Map.toSeq |> dict

[<RequireQualifiedAccess>]
module Map =
    let merge m1 m2 =
        Map.fold (fun s k v -> Map.add k v s) m1 m2
