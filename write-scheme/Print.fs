namespace WriteScheme

open Type

module Print =
    let formatFloat x isImaginary =
        if System.Double.IsNaN x then
            "+nan.0"
        elif System.Double.IsPositiveInfinity x then
            "+inf.0"
        elif System.Double.IsNegativeInfinity x then
            "-inf.0"
        else if isImaginary then
            x |> sprintf (if x >= 0.0 then "+%g" else "%g")
        else
            string x

    let formatComplex (x: System.Numerics.Complex) =
        let real =
            if x.Real = 0.0 && x.Imaginary <> 0.0 then
                "0"
            else
                formatFloat x.Real false

        let imag = formatFloat x.Imaginary true + "i"
        real + imag

    let formatString data =
        let sb = System.Text.StringBuilder data.runes.Length
        sb.Append '"' |> ignore

        for r in data.runes do
            match r |> string with
            | "\"" -> sb.Append "\\\"" |> ignore
            | x -> sb.Append x |> ignore

        sb.Append '"' |> ignore
        sb |> string

    let formatChar (c: System.Text.Rune) =
        match c.Value with
        | 32 -> "#\\ "
        | 10 -> "#\\newline"
        | 13 -> "#\\return"
        | 9 -> "#\\tab"
        | 7 -> "#\\alarm"
        | 8 -> "#\\backspace"
        | 127 -> "#\\delete"
        | 27 -> "#\\escape"
        | 0 -> "#\\null"
        | _ ->
            if System.Text.Rune.IsControl c then
                c.Value |> sprintf "#\\x%x"
            else
                c |> string |> sprintf "#\\%s"

    [<TailCall>]
    let rec formatList visited cont =
        function
        | [] -> "" |> cont
        | [ x ] -> x |> printCPS visited cont
        | x :: xs ->
            x
            |> printCPS visited (fun s1 -> xs |> formatList visited (fun s2 -> s1 + " " + s2 |> cont))

    and [<TailCall>] formatPair visited cont acc pair =
        if visited |> List.exists (fun p -> obj.ReferenceEquals(p, pair)) then
            match acc with
            | [] -> "..." |> cont
            | _ -> acc |> List.rev |> formatList visited (fun s -> s |> sprintf "(%s ...)" |> cont)
        else
            let visited' = pair :: visited

            match pair.cdr with
            | SEmpty, _ ->
                pair.car :: acc
                |> List.rev
                |> formatList visited' (fun s -> s |> sprintf "(%s)" |> cont)
            | SPair p, _ -> p |> formatPair visited' cont (pair.car :: acc)
            | _ ->
                pair.car :: acc
                |> List.rev
                |> formatList visited' (fun s1 ->
                    pair.cdr |> printCPS visited' (fun s2 -> sprintf "(%s . %s)" s1 s2 |> cont))

    and [<TailCall>] printCPS visited cont =
        function
        | SUnspecified, _ -> "#<unspecified>" |> cont
        | SEmpty, _ -> "()" |> cont
        | SBool true, _ -> "#t" |> cont
        | SBool false, _ -> "#f" |> cont
        | SRational(k, d), _ -> (if d = 1I then string k else sprintf "%A/%A" k d) |> cont
        | SReal x, _ -> formatFloat x false |> cont
        | SComplex x, _ -> formatComplex x |> cont
        | SString data, _ -> formatString data |> cont
        | SChar x, _ -> formatChar x |> cont
        | SSymbol x, _ -> x |> cont
        | SPair p, _ -> p |> formatPair visited cont []
        | SVector xs, _ -> xs |> Array.toList |> formatList visited (fun s -> s |> sprintf "#(%s)" |> cont)
        | SByteVector xs, _ -> xs |> Array.map string |> String.concat " " |> sprintf "#u8(%s)" |> cont
        | SValues xs, _ -> xs |> formatList visited (fun s -> s |> sprintf "(values %s)" |> cont)
        | SRecord(_, typeName, _), _ -> typeName |> sprintf "#<%s>" |> cont
        | SError(msg, irritants), _ ->
            let prefix = msg.runes |> runesToString |> sprintf "#<error \"%s\""

            match irritants with
            | [] -> prefix + ">" |> cont
            | _ -> irritants |> formatList visited (fun s -> prefix + " " + s + ">" |> cont)
        | SQuote x, _ -> x |> printCPS visited (fun s -> s |> sprintf "'%s" |> cont)
        | SQuasiquote x, _ -> x |> printCPS visited (fun s -> s |> sprintf "`%s" |> cont)
        | SUnquote x, _ -> x |> printCPS visited (fun s -> s |> sprintf ",%s" |> cont)
        | SUnquoteSplicing x, _ -> x |> printCPS visited (fun s -> s |> sprintf ",@%s" |> cont)
        | SPromise _, _ -> "#<promise>" |> cont
        | SParameter _, _ -> "#<parameter>" |> cont
        | SSyntax _, _ -> "#<syntax>" |> cont
        | SProcedure _, _ -> "#<procedure>" |> cont
        | SContinuation _, _ -> "#<continuation>" |> cont

    let printList xs = xs |> formatList [] id
    let print x = printList [ x ]
