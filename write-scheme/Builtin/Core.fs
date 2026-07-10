namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Core =
    let isEqv context pos cont =
        function
        | [ x; y ] -> Ok((x, y) |> eqv |> toSBool, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid eqv? parameter." |> cont

    [<TailCall>]
    let rec zipVectorEqual (x: SExpression array) (y: SExpression array) i acc =
        if i < 0 then
            acc
        else
            zipVectorEqual x y (i - 1) ((x.[i], y.[i]) :: acc)

    let isVisited visited a b =
        visited
        |> List.exists (fun (va, vb) ->
            LanguagePrimitives.PhysicalEquality va a
            && LanguagePrimitives.PhysicalEquality vb b)

    [<TailCall>]
    let rec loopEqual (visited: (obj * obj) list) =
        function
        | [] -> true
        | (x, y) :: rest ->
            if eqv (x, y) then
                rest |> loopEqual visited
            else
                match x, y with
                | (SPair a, _), (SPair b, _) -> equalPair visited rest a b
                | (SVector a, _), (SVector b, _) -> equalVector visited rest a b
                | (SByteVector a, _), (SByteVector b, _) -> equalByteVector visited rest a b
                | (SValues a, _), (SValues b, _) -> equalValues visited rest a b
                | (SQuote a, _), (SQuote b, _) -> equalQuoteLike visited rest a b
                | (SUnquote a, _), (SUnquote b, _) -> equalQuoteLike visited rest a b
                | (SString a, _), (SString b, _) -> equalString visited rest a b
                | _ -> false

    and equalPair visited rest (a: SPairData) (b: SPairData) =
        if isVisited visited a b then
            rest |> loopEqual visited
        else
            (a.car, b.car) :: (a.cdr, b.cdr) :: rest |> loopEqual ((a, b) :: visited)

    and equalVector visited rest (a: SExpression array) (b: SExpression array) =
        if isVisited visited a b then
            rest |> loopEqual visited
        elif a.Length <> b.Length then
            false
        else
            zipVectorEqual a b (a.Length - 1) rest |> loopEqual ((a, b) :: visited)

    and equalByteVector visited rest (a: byte array) (b: byte array) =
        if a = b then rest |> loopEqual visited else false

    and equalValues visited rest (a: SExpression list) (b: SExpression list) =
        if a.Length <> b.Length then
            false
        else
            List.zip a b @ rest |> loopEqual visited

    and equalQuoteLike visited rest (a: SExpression) (b: SExpression) = (a, b) :: rest |> loopEqual visited

    and equalString visited rest (a: SStringData) (b: SStringData) =
        if a.runes = b.runes then
            rest |> loopEqual visited
        else
            false

    let isEqual context pos cont =
        function
        | [ x; y ] -> Ok([ x, y ] |> loopEqual [] |> toSBool, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid equal? parameter." |> cont

    let readAndEvalFile context f p =
        readAndResolveInclude false f p
        |> Result.bind (fun exprs' -> exprs' |> mapResult (Eval.eval context id))

    let sLoad context pos cont =
        function
        | [ SString f, p ] ->
            readAndEvalFile context f p
            |> Result.map (fun _ -> f.runes |> runesToString |> (fun s -> $"Loaded '{s}'.") |> SSymbol, pos)
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid load parameter." |> cont
