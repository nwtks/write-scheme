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
        let runes = data.runes
        let sb = System.Text.StringBuilder runes.Length
        sb.Append '"' |> ignore

        for r in runes do
            let s = r.ToString()

            match s with
            | "\"" -> sb.Append "\\\"" |> ignore
            | _ -> sb.Append s |> ignore

        sb.Append '"' |> ignore
        sb.ToString()

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
                c.ToString() |> sprintf "#\\%s"

    [<TailCall>]
    let rec printListCPS cont =
        function
        | [] -> "" |> cont
        | [ x ] -> x |> printCPS cont
        | x :: xs -> x |> printCPS (fun s1 -> xs |> printListCPS (fun s2 -> s1 + " " + s2 |> cont))

    and [<TailCall>] printCPS cont =
        function
        | SUnspecified -> "#<unspecified>" |> cont
        | SEmpty -> "()" |> cont
        | SBool true -> "#t" |> cont
        | SBool false -> "#f" |> cont
        | SRational(k, d) -> (if d = 1I then string k else sprintf "%A/%A" k d) |> cont
        | SReal x -> formatFloat x false |> cont
        | SComplex x -> formatComplex x |> cont
        | SString data -> formatString data |> cont
        | SChar x -> formatChar x |> cont
        | SSymbol x -> x |> cont
        | SList xs -> xs |> printListCPS (fun s -> s |> sprintf "(%s)" |> cont)
        | SPair(x1, x2) ->
            match x2 with
            | SEmpty -> x1 |> toSList |> printCPS cont
            | SList x2' -> x1 @ x2' |> toSList |> printCPS cont
            | SPair(y1, y2) -> SPair(x1 @ y1, y2) |> printCPS cont
            | _ ->
                x1
                |> printListCPS (fun s1 -> x2 |> printCPS (fun s2 -> sprintf "(%s . %s)" s1 s2 |> cont))
        | SVector xs -> xs |> Array.toList |> printListCPS (fun s -> s |> sprintf "#(%s)" |> cont)
        | SByteVector xs -> xs |> Array.map string |> String.concat " " |> sprintf "#u8(%s)" |> cont
        | SValues xs -> xs |> printListCPS (fun s -> s |> sprintf "(values %s)" |> cont)
        | SRecord(_, typeName, _) -> typeName |> sprintf "#<%s>" |> cont
        | SError(msg, irritants) ->
            let prefix = msg.runes |> runesToString |> sprintf "#<error \"%s\""

            match irritants with
            | [] -> prefix + ">" |> cont
            | _ -> irritants |> printListCPS (fun s -> prefix + " " + s + ">" |> cont)
        | SQuote x -> x |> printCPS (fun s -> s |> sprintf "'%s" |> cont)
        | SQuasiquote x -> x |> printCPS (fun s -> s |> sprintf "`%s" |> cont)
        | SUnquote x -> x |> printCPS (fun s -> s |> sprintf ",%s" |> cont)
        | SUnquoteSplicing x -> x |> printCPS (fun s -> s |> sprintf ",@%s" |> cont)
        | SPromise _ -> "#<promise>" |> cont
        | SParameter _ -> "#<parameter>" |> cont
        | SSyntax _ -> "#<syntax>" |> cont
        | SProcedure _ -> "#<procedure>" |> cont
        | SContinuation _ -> "#<continuation>" |> cont

    let print x = x |> printCPS id
    let printList xs = xs |> printListCPS id
