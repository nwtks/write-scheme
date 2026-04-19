namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Symbol =
    let isSymbol envs cont =
        function
        | [ SSymbol _ ] -> STrue |> cont
        | [ _ ] -> SFalse |> cont
        | x -> x |> invalidParameter "'%s' invalid symbol? parameter."

    let isSymbolEq envs cont args =
        args
        |> List.pairwise
        |> List.forall (fun (a, b) ->
            match a, b with
            | SSymbol s1, SSymbol s2 -> s1 = s2
            | _ -> false)
        |> toSBool
        |> cont

    let sSymbolToString envs cont =
        function
        | [ SSymbol s ] -> s |> newSString true |> cont
        | x -> x |> invalidParameter "'%s' invalid symbol->string parameter."

    let sStringToSymbol envs cont =
        function
        | [ SString s ] -> s.runes |> runesToString |> SSymbol |> cont
        | x -> x |> invalidParameter "'%s' invalid string->symbol parameter."
