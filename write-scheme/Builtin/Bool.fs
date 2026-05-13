namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Bool =
    let sNot context pos cont =
        function
        | [ SBool false, _ ] -> Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid not parameter." |> cont

    let isBoolean context pos cont =
        function
        | [ SBool _, _ ] -> Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid boolean? parameter." |> cont

    let sBooleanEq context pos cont =
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
