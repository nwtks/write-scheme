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

        data.runes
        |> Seq.iter (
            string
            >> function
                | "\"" -> sb.Append "\\\""
                | "\\" -> sb.Append "\\\\"
                | x -> sb.Append x
            >> ignore
        )

        sb.Append '"' |> ignore
        sb |> string

    let formatChar (c: System.Text.Rune) =
        match c.Value with
        | 32 -> "#\\space"
        | 10 -> "#\\newline"
        | 13 -> "#\\return"
        | 9 -> "#\\tab"
        | 7 -> "#\\alarm"
        | 8 -> "#\\backspace"
        | 127 -> "#\\delete"
        | 27 -> "#\\escape"
        | 0 -> "#\\null"
        | x when System.Text.Rune.IsControl c -> x |> sprintf "#\\x%x"
        | _ -> c |> string |> sprintf "#\\%s"

    let isVisited visited x =
        visited |> List.exists (fun v -> obj.ReferenceEquals(v, x))

    [<TailCall>]
    let rec formatList next =
        function
        | [] -> "" |> next
        | [ visited, x ] -> x |> printCPS visited next
        | (visited, x) :: xs ->
            x
            |> printCPS visited (fun s1 -> xs |> formatList (fun s2 -> s1 + " " + s2 |> next))

    and [<TailCall>] formatPair visited next acc pair =
        if isVisited visited pair then
            match acc with
            | [] -> "..." |> next
            | _ -> acc |> List.rev |> formatList (sprintf "(%s ...)" >> next)
        else
            let visited' = (pair :> obj) :: visited

            match pair.cdr with
            | SEmpty, _ -> (visited', pair.car) :: acc |> List.rev |> formatList (sprintf "(%s)" >> next)
            | SPair p, _ -> p |> formatPair visited' next ((visited', pair.car) :: acc)
            | _ ->
                (visited', pair.car) :: acc
                |> List.rev
                |> formatList (fun s1 -> pair.cdr |> printCPS visited' (fun s2 -> sprintf "(%s . %s)" s1 s2 |> next))

    and [<TailCall>] printCPS visited next =
        function
        | SUnspecified, _ -> "#<unspecified>" |> next
        | SEmpty, _ -> "()" |> next
        | SBool true, _ -> "#t" |> next
        | SBool false, _ -> "#f" |> next
        | SRational(n, d), _ when d = 1I -> string n |> next
        | SRational(n, d), _ -> sprintf "%A/%A" n d |> next
        | SReal x, _ -> formatFloat x false |> next
        | SComplex x, _ -> formatComplex x |> next
        | SString data, _ -> formatString data |> next
        | SChar x, _ -> formatChar x |> next
        | SSymbol x, _ -> x |> next
        | SPair p, _ -> p |> formatPair visited next []
        | SVector xs, _ when isVisited visited xs -> "..." |> next
        | SVector xs, _ ->
            xs
            |> Array.toList
            |> List.map (fun e -> (xs :> obj) :: visited, e)
            |> formatList (sprintf "#(%s)" >> next)
        | SByteVector xs, _ -> xs |> Array.map string |> String.concat " " |> sprintf "#u8(%s)" |> next
        | SValues xs, _ when isVisited visited xs -> "..." |> next
        | SValues xs, _ ->
            xs
            |> List.map (fun e -> (xs :> obj) :: visited, e)
            |> formatList (sprintf "(values %s)" >> next)
        | SRecord(_, typeName, _), _ -> typeName |> sprintf "#<%s>" |> next
        | SError(msg, irritants), _ ->
            let prefix = msg.runes |> runesToString |> sprintf "#<error \"%s\""

            match irritants with
            | [] -> prefix + ">" |> next
            | _ when isVisited visited irritants -> "..." |> next
            | _ ->
                irritants
                |> List.map (fun e -> (irritants :> obj) :: visited, e)
                |> formatList (fun s -> prefix + " " + s + ">" |> next)
        | SQuote x, _ -> x |> printCPS visited (sprintf "'%s" >> next)
        | SQuasiquote x, _ -> x |> printCPS visited (sprintf "`%s" >> next)
        | SUnquote x, _ -> x |> printCPS visited (sprintf ",%s" >> next)
        | SUnquoteSplicing x, _ -> x |> printCPS visited (sprintf ",@%s" >> next)
        | SDatumLabel(n, d), _ -> d |> printCPS visited (sprintf "#%d=%s" n >> next)
        | SDatumRef n, _ -> sprintf "#%d#" n |> next
        | SPromise _, _ -> "#<promise>" |> next
        | SParameter _, _ -> "#<parameter>" |> next
        | SSyntax _, _ -> "#<syntax>" |> next
        | SProcedure _, _ -> "#<procedure>" |> next
        | SContinuation _, _ -> "#<continuation>" |> next

    let printList xs =
        xs |> List.map (fun x -> [], x) |> formatList id

    let print x = printList [ x ]
