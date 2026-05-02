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
    let rec formatList visited next =
        function
        | [] -> "" |> next
        | [ x ] -> x |> printCPS visited next
        | x :: xs ->
            x
            |> printCPS visited (fun s1 -> xs |> formatList visited (fun s2 -> s1 + " " + s2 |> next))

    and [<TailCall>] formatPair visited next acc pair =
        if visited |> List.exists (fun p -> obj.ReferenceEquals(p, pair)) then
            match acc with
            | [] -> "..." |> next
            | _ -> acc |> List.rev |> formatList visited (fun s -> s |> sprintf "(%s ...)" |> next)
        else
            let visited' = pair :: visited

            match pair.cdr with
            | SEmpty, _ ->
                pair.car :: acc
                |> List.rev
                |> formatList visited' (fun s -> s |> sprintf "(%s)" |> next)
            | SPair p, _ -> p |> formatPair visited' next (pair.car :: acc)
            | _ ->
                pair.car :: acc
                |> List.rev
                |> formatList visited' (fun s1 ->
                    pair.cdr |> printCPS visited' (fun s2 -> sprintf "(%s . %s)" s1 s2 |> next))

    and [<TailCall>] printCPS visited next =
        function
        | SUnspecified, _ -> "#<unspecified>" |> next
        | SEmpty, _ -> "()" |> next
        | SBool true, _ -> "#t" |> next
        | SBool false, _ -> "#f" |> next
        | SRational(k, d), _ -> (if d = 1I then string k else sprintf "%A/%A" k d) |> next
        | SReal x, _ -> formatFloat x false |> next
        | SComplex x, _ -> formatComplex x |> next
        | SString data, _ -> formatString data |> next
        | SChar x, _ -> formatChar x |> next
        | SSymbol x, _ -> x |> next
        | SPair p, _ -> p |> formatPair visited next []
        | SVector xs, _ -> xs |> Array.toList |> formatList visited (fun s -> s |> sprintf "#(%s)" |> next)
        | SByteVector xs, _ -> xs |> Array.map string |> String.concat " " |> sprintf "#u8(%s)" |> next
        | SValues xs, _ -> xs |> formatList visited (fun s -> s |> sprintf "(values %s)" |> next)
        | SRecord(_, typeName, _), _ -> typeName |> sprintf "#<%s>" |> next
        | SError(msg, irritants), _ ->
            let prefix = msg.runes |> runesToString |> sprintf "#<error \"%s\""

            match irritants with
            | [] -> prefix + ">" |> next
            | _ -> irritants |> formatList visited (fun s -> prefix + " " + s + ">" |> next)
        | SQuote x, _ -> x |> printCPS visited (fun s -> s |> sprintf "'%s" |> next)
        | SQuasiquote x, _ -> x |> printCPS visited (fun s -> s |> sprintf "`%s" |> next)
        | SUnquote x, _ -> x |> printCPS visited (fun s -> s |> sprintf ",%s" |> next)
        | SUnquoteSplicing x, _ -> x |> printCPS visited (fun s -> s |> sprintf ",@%s" |> next)
        | SDatumLabel(n, d), _ -> d |> printCPS visited (fun s -> s |> sprintf "#%d=%s" n |> next)
        | SDatumRef n, _ -> sprintf "#%d#" n |> next
        | SPromise _, _ -> "#<promise>" |> next
        | SParameter _, _ -> "#<parameter>" |> next
        | SSyntax _, _ -> "#<syntax>" |> next
        | SProcedure _, _ -> "#<procedure>" |> next
        | SContinuation _, _ -> "#<continuation>" |> next

    let printList xs = xs |> formatList [] id
    let print x = printList [ x ]
