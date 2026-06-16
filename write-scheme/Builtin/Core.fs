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
                | (SPair a, _), (SPair b, _) ->
                    if isVisited visited a b then
                        rest |> loopEqual visited
                    else
                        (a.car, b.car) :: (a.cdr, b.cdr) :: rest |> loopEqual ((a, b) :: visited)
                | (SVector a, _), (SVector b, _) ->
                    if isVisited visited a b then
                        rest |> loopEqual visited
                    else if a.Length <> b.Length then
                        false
                    else
                        zipVectorEqual a b (a.Length - 1) rest |> loopEqual ((a, b) :: visited)
                | (SByteVector a, _), (SByteVector b, _) -> a = b && rest |> loopEqual visited
                | (SValues a, _), (SValues b, _) ->
                    if a.Length <> b.Length then
                        false
                    else
                        List.zip a b @ rest |> loopEqual visited
                | (SQuote a, _), (SQuote b, _) -> (a, b) :: rest |> loopEqual visited
                | (SUnquote a, _), (SUnquote b, _) -> (a, b) :: rest |> loopEqual visited
                | (SString a, _), (SString b, _) -> a.runes = b.runes && rest |> loopEqual visited
                | _ -> false

    let isEqual context pos cont =
        function
        | [ x; y ] -> Ok([ x, y ] |> loopEqual [] |> toSBool, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid equal? parameter." |> cont

    let sDisplay context pos cont =
        function
        | [ SString x, _ ] ->
            x.runes |> runesToString |> printf "%s"
            Ok(SUnspecified, pos) |> cont
        | [ SChar x, _ ] ->
            x |> string |> printf "%s"
            Ok(SUnspecified, pos) |> cont
        | [ x ] ->
            x |> Print.print |> printf "%s"
            Ok(SUnspecified, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid display parameter." |> cont

    let sLoad context pos cont =
        function
        | [ SString f, p ] ->
            match tryReadAll false f p with
            | Ok exprs ->
                match exprs |> mapResult DatumLabel.resolveLabels with
                | Ok exprs' ->
                    match exprs' |> mapResult (Eval.eval context id) with
                    | Ok _ -> Ok(f.runes |> runesToString |> sprintf "Loaded '%s'." |> SSymbol, pos) |> cont
                    | Error e -> Error e |> cont
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid load parameter." |> cont
