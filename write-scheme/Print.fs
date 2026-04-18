namespace WriteScheme

open Type

module Print =
    let namedChars =
        [ "\u0007", "alarm"
          "\u0008", "backspace"
          "\u007f", "delete"
          "\u001b", "escape"
          "\u000a", "newline"
          "\u0000", "null"
          "\u000d", "return"
          "\u0009", "tab" ]
        |> Map.ofList

    let formatFloat x isImaginary =
        if System.Double.IsNaN x then
            "+nan.0"
        elif System.Double.IsPositiveInfinity x then
            "+inf.0"
        elif System.Double.IsNegativeInfinity x then
            "-inf.0"
        else
            let s = string x
            if isImaginary && x >= 0.0 then "+" + s else s

    let rec print =
        function
        | SUnspecified -> "#<unspecified>"
        | SEmpty -> "()"
        | SBool true -> "#t"
        | SBool false -> "#f"
        | SRational(x1, x2) -> if x2 = 1I then string x1 else sprintf "%A/%A" x1 x2
        | SReal x -> formatFloat x false
        | SComplex x -> sprintf "%s%si" (formatFloat x.Real false) (formatFloat x.Imaginary true)
        | SString x -> x.Replace("\"", "\\\"") |> sprintf "\"%s\""
        | SChar x ->
            match Map.tryFind x namedChars with
            | Some name -> sprintf "#\\%s" name
            | None ->
                if x.Length = 1 && System.Char.IsControl(x.[0]) then
                    sprintf "#\\x%x" (int x.[0])
                else
                    sprintf "#\\%s" x
        | SSymbol x -> x
        | SList xs -> printList xs |> sprintf "(%s)"
        | SPair(x1, x2) ->
            match x2 with
            | SEmpty -> SList x1 |> print
            | SList x2' -> SList(x1 @ x2') |> print
            | SPair(y1, y2) -> SPair(x1 @ y1, y2) |> print
            | _ -> sprintf "(%s . %s)" (printList x1) (print x2)
        | SVector xs -> xs |> Array.map print |> String.concat " " |> sprintf "#(%s)"
        | SByteVector xs -> xs |> Array.map string |> String.concat " " |> sprintf "#u8(%s)"
        | SValues xs -> xs |> List.map print |> String.concat " " |> sprintf "(values %s)"
        | SRecord(_, typeName, _) -> sprintf "#<%s>" typeName
        | SError(msg, []) -> sprintf "#<error \"%s\">" msg
        | SError(msg, irritants) -> sprintf "#<error \"%s\" %s>" msg (printList irritants)
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
