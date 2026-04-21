namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Bool =
    let sNot envs cont =
        function
        | [ SBool false ] -> STrue |> cont
        | _ -> SFalse |> cont

    let isBoolean envs cont =
        function
        | [ SBool _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    let isBooleanEq envs cont args =
        args
        |> List.map (function
            | SBool b -> b
            | x -> [ x ] |> invalidParameter "'%s' is not a boolean in boolean=?.")
        |> List.pairwise
        |> List.forall (fun (a, b) -> a = b)
        |> toSBool
        |> cont
