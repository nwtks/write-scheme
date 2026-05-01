namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Symbol =
    let isSymbol envs pos cont =
        function
        | [ SSymbol _, _ ] -> (STrue, pos) |> cont
        | [ _ ] -> (SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid symbol? parameter."

    let isSymbolEq envs pos cont =
        List.pairwise
        >> List.forall (fun (a, b) ->
            match a, b with
            | (SSymbol s1, _), (SSymbol s2, _) -> s1 = s2
            | _ -> false)
        >> toSBool
        >> fun x -> x, pos
        >> cont

    let sSymbolToString envs pos cont =
        function
        | [ SSymbol s, _ ] -> (s |> newSString true, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid symbol->string parameter."

    let sStringToSymbol envs pos cont =
        function
        | [ SString s, _ ] -> (s.runes |> runesToString |> SSymbol, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string->symbol parameter."
