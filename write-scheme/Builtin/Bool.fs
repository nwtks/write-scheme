namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Bool =
    let sNot envs pos cont =
        function
        | [ SBool false, _ ] -> (STrue, pos) |> cont
        | _ -> (SFalse, pos) |> cont

    let isBoolean envs pos cont =
        function
        | [ SBool _, _ ] -> (STrue, pos) |> cont
        | _ -> (SFalse, pos) |> cont

    let isBooleanEq envs pos cont =
        List.map (function
            | SBool b, _ -> b
            | x -> x |> invalid (snd x) "'%s' is not a boolean in boolean=?.")
        >> List.pairwise
        >> List.forall (fun (a, b) -> a = b)
        >> toSBool
        >> fun x -> x, pos
        >> cont
