namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Core =
    let isEqv envs cont =
        function
        | [ a; b ] -> (a, b) |> eqv |> toSBool |> cont
        | _ -> SFalse |> cont

    [<TailCall>]
    let rec loopEqual =
        function
        | [] -> true
        | (a, b) :: xs ->
            match a, b with
            | SPair pa, SPair pb -> (pa.car, pb.car) :: (pa.cdr, pb.cdr) :: xs |> loopEqual
            | SVector va, SVector vb ->
                if va.Length <> vb.Length then
                    false
                else
                    let rec zip i acc =
                        if i < 0 then acc else zip (i - 1) ((va.[i], vb.[i]) :: acc)

                    zip (va.Length - 1) xs |> loopEqual
            | SByteVector va, SByteVector vb ->
                if va.Length <> vb.Length then
                    false
                else
                    let rec loop i =
                        if i < 0 then true
                        elif va.[i] <> vb.[i] then false
                        else loop (i - 1)

                    loop (va.Length - 1) && loopEqual xs
            | SValues va, SValues vb ->
                if va.Length <> vb.Length then
                    false
                else
                    List.zip va vb @ xs |> loopEqual
            | SQuote a', SQuote b' -> (a', b') :: xs |> loopEqual
            | SUnquote a', SUnquote b' -> (a', b') :: xs |> loopEqual
            | SBool a', SBool b' -> a' = b' && loopEqual xs
            | SRational(a1, a2), SRational(b1, b2) -> a1 = b1 && a2 = b2 && loopEqual xs
            | SReal a', SReal b' -> a' = b' && loopEqual xs
            | SComplex a', SComplex b' -> a' = b' && loopEqual xs
            | SString a', SString b' ->
                a'.runes.Length = b'.runes.Length
                && Array.forall2 (=) a'.runes b'.runes
                && loopEqual xs
            | SChar a', SChar b' -> a' = b' && loopEqual xs
            | SSymbol a', SSymbol b' -> a' = b' && loopEqual xs
            | (a', _), (b', _) -> a' = b' && loopEqual xs

    let isEqual envs cont =
        function
        | [ a; b ] -> [ a, b ] |> loopEqual |> toSBool |> cont
        | _ -> SFalse |> cont

    let sDisplay envs cont =
        function
        | [ SString x ] ->
            x.runes |> runesToString |> printf "%s"
            SEmpty |> cont
        | [ SChar x ] ->
            x.ToString() |> printf "%s"
            SEmpty |> cont
        | [ x ] ->
            x |> Print.print |> printf "%s"
            SEmpty |> cont
        | x -> x |> invalidParameter "'%s' invalid display parameter."

    let sLoad envs cont =
        function
        | [ SString f ] ->
            let path = f.runes |> runesToString
            System.IO.File.ReadAllText path |> Read.read |> Eval.eval envs cont |> ignore
            sprintf "Loaded '%s'." path |> SSymbol |> cont
        | x -> x |> invalidParameter "'%s' invalid load parameter."
