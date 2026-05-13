namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Core =
    let isEqv context pos cont =
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
        | (x, y) :: rest ->
            match x, y with
            | (SPair a, _), (SPair b, _) -> (a.car, b.car) :: (a.cdr, b.cdr) :: rest |> loopEqual
            | (SVector a, _), (SVector b, _) ->
                if a.Length <> b.Length then
                    false
                else
                    zipVectorEqual a b (a.Length - 1) rest |> loopEqual
            | (SByteVector a, _), (SByteVector b, _) ->
                if a.Length <> b.Length then
                    false
                else
                    byteVectorEqual a b (a.Length - 1) && rest |> loopEqual
            | (SValues a, _), (SValues b, _) ->
                if a.Length <> b.Length then
                    false
                else
                    List.zip a b @ rest |> loopEqual
            | (SQuote a, _), (SQuote b, _) -> (a, b) :: rest |> loopEqual
            | (SUnquote a, _), (SUnquote b, _) -> (a, b) :: rest |> loopEqual
            | (SBool a, _), (SBool b, _) -> a = b && rest |> loopEqual
            | (SRational(n1, d1), _), (SRational(n2, d2), _) -> n1 = n2 && d1 = d2 && rest |> loopEqual
            | (SReal r1, _), (SReal r2, _) -> r1 = r2 && rest |> loopEqual
            | (SComplex c1, _), (SComplex c2, _) -> c1 = c2 && rest |> loopEqual
            | (SString a, _), (SString b, _) ->
                a.runes.Length = b.runes.Length
                && Array.forall2 (=) a.runes b.runes
                && rest |> loopEqual
            | (SChar a, _), (SChar b, _) -> a = b && rest |> loopEqual
            | (SSymbol a, _), (SSymbol b, _) -> a = b && rest |> loopEqual
            | (a, _), (b, _) -> a = b && rest |> loopEqual

    let isEqual context pos cont =
        function
        | [ x; y ] -> Ok([ x, y ] |> loopEqual |> toSBool, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

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
