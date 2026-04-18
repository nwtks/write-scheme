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
            | SList la, SList lb ->
                if la.Length <> lb.Length then
                    false
                else
                    List.zip la lb @ xs |> loopEqual
            | SPair(la, ra), SPair(lb, rb) ->
                if la.Length <> lb.Length then
                    false
                else
                    List.zip la lb @ (ra, rb) :: xs |> loopEqual
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
            | SString a', SString b' -> a' = b' && loopEqual xs
            | SChar a', SChar b' -> a' = b' && loopEqual xs
            | SSymbol a', SSymbol b' -> a' = b' && loopEqual xs
            | a', b' -> a' = b' && loopEqual xs

    let isEqual envs cont =
        function
        | [ a; b ] -> [ a, b ] |> loopEqual |> toSBool |> cont
        | _ -> SFalse |> cont

    let sNot envs cont =
        function
        | [ SBool false ] -> STrue |> cont
        | _ -> SFalse |> cont

    let isBoolean envs cont =
        function
        | [ SBool _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    let sDisplay envs cont =
        function
        | [ SString x ] ->
            x |> printf "%s"
            SEmpty |> cont
        | [ SChar x ] ->
            x |> printf "%s"
            SEmpty |> cont
        | [ x ] ->
            x |> Print.print |> printf "%s"
            SEmpty |> cont
        | x -> x |> invalidParameter "'%s' invalid display parameter."

    let sLoad envs cont =
        function
        | [ SString f ] ->
            System.IO.File.ReadAllText f |> Read.read |> Eval.eval envs cont |> ignore
            sprintf "Loaded '%s'." f |> SSymbol |> cont
        | x -> x |> invalidParameter "'%s' invalid load parameter."
