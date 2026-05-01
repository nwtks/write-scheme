namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Core =
    let isEqv envs pos cont =
        function
        | [ a; b ] -> ((a, b) |> eqv |> toSBool, pos) |> cont
        | _ -> (SFalse, pos) |> cont

    [<TailCall>]
    let rec zipVectorEqual (a: SExpression array) (b: SExpression array) i acc =
        if i < 0 then
            acc
        else
            zipVectorEqual a b (i - 1) ((a.[i], b.[i]) :: acc)

    [<TailCall>]
    let rec loopByteVectorEqual (a: byte array) (b: byte array) i =
        if i < 0 then true
        elif a.[i] <> b.[i] then false
        else loopByteVectorEqual a b (i - 1)

    [<TailCall>]
    let rec loopEqual =
        function
        | [] -> true
        | (a, b) :: xs ->
            match a, b with
            | (SPair pa, _), (SPair pb, _) -> (pa.car, pb.car) :: (pa.cdr, pb.cdr) :: xs |> loopEqual
            | (SVector va, _), (SVector vb, _) ->
                if va.Length <> vb.Length then
                    false
                else
                    zipVectorEqual va vb (va.Length - 1) xs |> loopEqual
            | (SByteVector va, _), (SByteVector vb, _) ->
                if va.Length <> vb.Length then
                    false
                else
                    loopByteVectorEqual va vb (va.Length - 1) && loopEqual xs
            | (SValues va, _), (SValues vb, _) ->
                if va.Length <> vb.Length then
                    false
                else
                    List.zip va vb @ xs |> loopEqual
            | (SQuote a', _), (SQuote b', _) -> (a', b') :: xs |> loopEqual
            | (SUnquote a', _), (SUnquote b', _) -> (a', b') :: xs |> loopEqual
            | (SBool a', _), (SBool b', _) -> a' = b' && loopEqual xs
            | (SRational(a1, a2), _), (SRational(b1, b2), _) -> a1 = b1 && a2 = b2 && loopEqual xs
            | (SReal a', _), (SReal b', _) -> a' = b' && loopEqual xs
            | (SComplex a', _), (SComplex b', _) -> a' = b' && loopEqual xs
            | (SString a', _), (SString b', _) ->
                a'.runes.Length = b'.runes.Length
                && Array.forall2 (=) a'.runes b'.runes
                && loopEqual xs
            | (SChar a', _), (SChar b', _) -> a' = b' && loopEqual xs
            | (SSymbol a', _), (SSymbol b', _) -> a' = b' && loopEqual xs
            | (a', _), (b', _) -> a' = b' && loopEqual xs

    let isEqual envs pos cont =
        function
        | [ a; b ] -> ([ a, b ] |> loopEqual |> toSBool, pos) |> cont
        | _ -> (SFalse, pos) |> cont

    let sDisplay envs pos cont =
        function
        | [ SString x, _ ] ->
            x.runes |> runesToString |> printf "%s"
            (SUnspecified, pos) |> cont
        | [ SChar x, _ ] ->
            x |> string |> printf "%s"
            (SUnspecified, pos) |> cont
        | [ x ] ->
            x |> Print.print |> printf "%s"
            (SUnspecified, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid display parameter."

    let sLoad envs pos cont =
        function
        | [ SString f, _ ] ->
            let path = f.runes |> runesToString
            path |> System.IO.File.ReadAllText |> Read.read |> Eval.eval envs cont |> ignore
            (path |> sprintf "Loaded '%s'." |> SSymbol, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid load parameter."
