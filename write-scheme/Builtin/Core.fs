namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Core =
    let isEqv envs pos cont =
        function
        | [ x; y ] -> Ok((x, y) |> eqv |> toSBool, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    [<TailCall>]
    let rec zipVectorEqual (x: SExpression array) (y: SExpression array) i acc =
        if i < 0 then
            acc
        else
            zipVectorEqual x y (i - 1) ((x.[i], y.[i]) :: acc)

    [<TailCall>]
    let rec byteVectorEqual (x: byte array) (y: byte array) i =
        if i < 0 then true
        elif x.[i] <> y.[i] then false
        else byteVectorEqual x y (i - 1)

    [<TailCall>]
    let rec loopEqual =
        function
        | [] -> true
        | (x, y) :: xs ->
            match x, y with
            | (SPair a, _), (SPair b, _) -> (a.car, b.car) :: (a.cdr, b.cdr) :: xs |> loopEqual
            | (SVector a, _), (SVector b, _) ->
                if a.Length <> b.Length then
                    false
                else
                    zipVectorEqual a b (a.Length - 1) xs |> loopEqual
            | (SByteVector a, _), (SByteVector b, _) ->
                if a.Length <> b.Length then
                    false
                else
                    byteVectorEqual a b (a.Length - 1) && loopEqual xs
            | (SValues a, _), (SValues b, _) ->
                if a.Length <> b.Length then
                    false
                else
                    List.zip a b @ xs |> loopEqual
            | (SQuote a, _), (SQuote b, _) -> (a, b) :: xs |> loopEqual
            | (SUnquote a, _), (SUnquote b, _) -> (a, b) :: xs |> loopEqual
            | (SBool a, _), (SBool b, _) -> a = b && loopEqual xs
            | (SRational(n1, d1), _), (SRational(n2, d2), _) -> n1 = n2 && d1 = d2 && loopEqual xs
            | (SReal r1, _), (SReal r2, _) -> r1 = r2 && loopEqual xs
            | (SComplex c1, _), (SComplex c2, _) -> c1 = c2 && loopEqual xs
            | (SString a, _), (SString b, _) ->
                a.runes.Length = b.runes.Length
                && Array.forall2 (=) a.runes b.runes
                && loopEqual xs
            | (SChar a, _), (SChar b, _) -> a = b && loopEqual xs
            | (SSymbol a, _), (SSymbol b, _) -> a = b && loopEqual xs
            | (a, _), (b, _) -> a = b && loopEqual xs

    let isEqual envs pos cont =
        function
        | [ x; y ] -> Ok([ x, y ] |> loopEqual |> toSBool, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let sDisplay envs pos cont =
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

    let sLoad envs pos cont =
        function
        | [ SString f, _ ] ->
            let path = f.runes |> runesToString

            path
            |> System.IO.File.ReadAllText
            |> Read.read
            |> Result.bind DatumLabel.resolveLabels
            |> Result.bind (Eval.eval envs id)
            |> Result.map (fun _ -> path |> sprintf "Loaded '%s'." |> SSymbol, pos)
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid load parameter." |> cont
