namespace WriteScheme

open Type

module Print =
    let rec print =
        function
        | SUnspecified -> "#<unspecified>"
        | SEmpty -> "()"
        | SBool true -> "#t"
        | SBool false -> "#f"
        | SRational(x1, x2) -> if x2 = 1I then string x1 else sprintf "%A/%A" x1 x2
        | SReal x ->
            if System.Double.IsNaN x then "+nan.0"
            elif System.Double.IsPositiveInfinity x then "+inf.0"
            elif System.Double.IsNegativeInfinity x then "-inf.0"
            else string x
        | SString x -> x.Replace("\"", "\\\"") |> sprintf "\"%s\""
        | SChar x -> sprintf "#\\%s" x
        | SSymbol x -> x
        | SError(msg, []) -> sprintf "#<error \"%s\">" msg
        | SError(msg, irritants) -> sprintf "#<error \"%s\" %s>" msg (printList irritants)
        | SValues xs -> xs |> List.map print |> String.concat " " |> sprintf "(values %s)"
        | SList xs -> xs |> printList |> sprintf "(%s)"
        | SVector xs -> xs |> Array.map print |> String.concat " " |> sprintf "#(%s)"
        | SPair(x1, x2) -> sprintf "(%s . %s)" (printList x1) (print x2)
        | SQuote x -> print x |> sprintf "'%s"
        | SQuasiquote x -> print x |> sprintf "`%s"
        | SUnquote x -> print x |> sprintf ",%s"
        | SUnquoteSplicing x -> print x |> sprintf ",@%s"
        | SPromise _ -> "#<promise>"
        | SParameter _ -> "#<parameter>"
        | SSyntax _ -> "#<syntax>"
        | SProcedure _ -> "#<procedure>"
        | SContinuation _ -> "#<continuation>"

    and printList xs =
        xs |> List.map print |> String.concat " "
