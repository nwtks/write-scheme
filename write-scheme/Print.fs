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
            sprintf (if x >= 0.0 then "+%g" else "%g") x
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
    let rec printList xs =
        xs |> List.map print |> String.concat " "

    and print =
        function
        | SUnspecified -> "#<unspecified>"
        | SEmpty -> "()"
        | SBool true -> "#t"
        | SBool false -> "#f"
        | SRational(k, d) -> if d = 1I then string k else sprintf "%A/%A" k d
        | SReal x -> formatFloat x false
        | SComplex x -> formatComplex x
        | SString data -> formatString data
        | SChar x -> formatChar x
        | SSymbol x -> x
        | SList xs -> xs |> printList |> sprintf "(%s)"
        | SPair(x1, x2) ->
            match x2 with
            | SEmpty -> x1 |> toSList |> print
            | SList x2' -> x1 @ x2' |> toSList |> print
            | SPair(y1, y2) -> SPair(x1 @ y1, y2) |> print
            | _ -> sprintf "(%s . %s)" (x1 |> printList) (x2 |> print)
        | SVector xs -> xs |> Array.map print |> String.concat " " |> sprintf "#(%s)"
        | SByteVector xs -> xs |> Array.map string |> String.concat " " |> sprintf "#u8(%s)"
        | SValues xs -> xs |> List.map print |> String.concat " " |> sprintf "(values %s)"
        | SRecord(_, typeName, _) -> sprintf "#<%s>" typeName
        | SError(msg, []) -> sprintf "#<error \"%s\">" (msg.runes |> runesToString)
        | SError(msg, irritants) -> sprintf "#<error \"%s\" %s>" (msg.runes |> runesToString) (irritants |> printList)
        | SQuote x -> x |> print |> sprintf "'%s"
        | SQuasiquote x -> x |> print |> sprintf "`%s"
        | SUnquote x -> x |> print |> sprintf ",%s"
        | SUnquoteSplicing x -> x |> print |> sprintf ",@%s"
        | SPromise _ -> "#<promise>"
        | SParameter _ -> "#<parameter>"
        | SSyntax _ -> "#<syntax>"
        | SProcedure _ -> "#<procedure>"
        | SContinuation _ -> "#<continuation>"
