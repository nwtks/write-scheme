namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Symbol =
    let isSymbol envs pos cont =
        function
        | [ SSymbol _, _ ] -> Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid symbol? parameter." |> cont

    let isSymbolEq envs pos cont =
        mapResult (function
            | SSymbol _, _ as sym -> Ok sym
            | x -> x |> invalid (snd x) "'%s' is not a symbol in symbol=?.")
        >> Result.map (
            List.pairwise
            >> List.forall (fun (a, b) ->
                match a, b with
                | (SSymbol s1, _), (SSymbol s2, _) -> s1 = s2
                | _ -> false)
            >> toSBool
            >> fun x -> x, pos
        )
        >> cont

    let sSymbolToString envs pos cont =
        function
        | [ SSymbol s, _ ] -> Ok(s |> newSString true, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid symbol->string parameter." |> cont

    let sStringToSymbol envs pos cont =
        function
        | [ SString s, _ ] -> Ok(s.runes |> runesToString |> SSymbol, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string->symbol parameter." |> cont
