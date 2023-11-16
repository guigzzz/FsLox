namespace Lox

open System.Collections.Generic

[<RequireQualifiedAccess>]
module Dictionary =
    let ofMap (map: Map<_, _>) : IDictionary<_, _> = map |> Map.toSeq |> dict

[<RequireQualifiedAccess>]
module Map =
    let merge m1 m2 =
        Map.fold (fun s k v -> Map.add k v s) m1 m2

    let mapValues f m = m |> Map.map (fun k v -> k, f v)

    let find' k m =
        let defaultThunk () =
            let keyStr = m |> Map.keys |> Seq.map string |> String.concat ", "
            failwith $"Failed to find {k} in map. Keys were: {keyStr}"

        m |> Map.tryFind k |> Option.defaultWith defaultThunk

[<CustomEquality; NoComparison>]
type StructurallyNull<'T> =
    { V: 'T }

    override x.Equals(yobj) =
        match yobj with
        | :? StructurallyNull<'T> -> true
        | _ -> false

    override x.GetHashCode() = 0

[<RequireQualifiedAccess>]
module StructurallyNull =
    let make v = { V = v }
