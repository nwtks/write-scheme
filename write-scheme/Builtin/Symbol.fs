namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Symbol =
    let isSymbol envs cont =
        function
        | [ SSymbol _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    let isSymbolEq envs cont =
        function
        | []
        | [ _ ] -> STrue |> cont
        | args ->
            args
            |> List.map (function
                | SSymbol s -> s
                | x -> Print.print x |> sprintf "'%s' is not a symbol in symbol=?." |> failwith)
            |> List.pairwise
            |> List.forall (fun (a, b) -> a = b)
            |> toSBool
            |> cont

    let sSymbolToString envs cont =
        function
        | [ SSymbol s ] -> SString s |> cont
        | x -> x |> invalidParameter "'%s' invalid symbol->string parameter."

    let sStringToSymbol envs cont =
        function
        | [ SString s ] -> SSymbol s |> cont
        | x -> x |> invalidParameter "'%s' invalid string->symbol parameter."
