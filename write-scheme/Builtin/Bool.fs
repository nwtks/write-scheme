namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Bool =
    let sNot envs pos cont =
        function
        | [ SBool false, _ ] -> Ok(STrue, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let isBoolean envs pos cont =
        function
        | [ SBool _, _ ] -> Ok(STrue, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let isBooleanEq envs pos cont =
        mapResult (function
            | SBool b, _ -> Ok b
            | x -> x |> invalid (snd x) "'%s' is not a boolean in boolean=?.")
        >> Result.map (
            List.pairwise
            >> List.forall (fun (a, b) -> a = b)
            >> toSBool
            >> fun x -> x, pos
        )
        >> cont
