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
                | "\"" -> "\\\""
                | "\\" -> "\\\\"
                | x -> x
            >> sb.Append
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


    let formatSymbol s =
        let isInitial c =
            System.Char.IsLetter c || "!$%&*/:<=>?^_~".Contains c

        let isSubsequent c =
            isInitial c || System.Char.IsDigit c || "+-.@".Contains c

        if s = "" then
            "||"
        elif s = "+" || s = "-" || s = "..." then
            s
        else
            let needsPipe =
                not (isInitial s.[0])
                && not (s.[0] = '+' || s.[0] = '-')
                && not (s.Length > 1 && s.[0] = '.' && (isInitial s.[1] || "+-.@".Contains(s.[1])))
                && not (s.StartsWith "...")
                || s |> Seq.exists (fun c -> not (isSubsequent c))

            if needsPipe then
                "|" + s.Replace("\\", "\\\\").Replace("|", "\\|") + "|"
            else
                s

    let isVisited visited x =
        visited |> List.exists (fun v -> obj.ReferenceEquals(v, x))

    let formatSimpleValue =
        function
        | SUnspecified, _ -> "#<unspecified>"
        | SEmpty, _ -> "()"
        | SBool b, _ -> if b then "#t" else "#f"
        | SRational(n, d), _ -> if d = 1I then string n else sprintf "%A/%A" n d
        | SReal x, _ -> formatFloat x false
        | SComplex x, _ -> formatComplex x
        | SString data, _ -> formatString data
        | SChar x, _ -> formatChar x
        | SSymbol x, _ -> formatSymbol x
        | SByteVector xs, _ -> xs |> Array.map string |> String.concat " " |> sprintf "#u8(%s)"
        | _ -> failwith "unreachable."

    let formatOpaqueDescriptor =
        function
        | SRecord(_, typeName, _), _ -> sprintf "#<%s>" typeName
        | SDatumRef n, _ -> sprintf "#%d#" n
        | SPromise _, _ -> "#<promise>"
        | SParameter _, _ -> "#<parameter>"
        | SSyntax _, _ -> "#<syntax>"
        | SProcedure _, _ -> "#<procedure>"
        | SContinuation _, _ -> "#<continuation>"
        | _ -> failwith "unreachable."

    let getWrapperPrefixAndInner =
        function
        | SQuote x, _ -> "'", x
        | SQuasiquote x, _ -> "`", x
        | SUnquote x, _ -> ",", x
        | SUnquoteSplicing x, _ -> ",@", x
        | SDatumLabel(n, d), _ -> sprintf "#%d=" n, d
        | _ -> failwith "unreachable."

    let isSimpleValueKind =
        function
        | SUnspecified
        | SEmpty
        | SBool _
        | SRational _
        | SReal _
        | SComplex _
        | SString _
        | SChar _
        | SSymbol _
        | SByteVector _ -> true
        | _ -> false

    let isOpaqueDescriptorKind =
        function
        | SRecord _
        | SDatumRef _
        | SPromise _
        | SParameter _
        | SSyntax _
        | SProcedure _
        | SContinuation _ -> true
        | _ -> false

    let isQuoteLikeKind =
        function
        | SQuote _
        | SQuasiquote _
        | SUnquote _
        | SUnquoteSplicing _
        | SDatumLabel _ -> true
        | _ -> false

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

    and [<TailCall>] formatError visited next message irritants =
        let prefix = message.runes |> runesToString |> sprintf "#<error \"%s\""

        match irritants with
        | [] -> prefix + ">" |> next
        | _ when isVisited visited irritants -> "..." |> next
        | _ ->
            irritants
            |> List.map (fun e -> (irritants :> obj) :: visited, e)
            |> formatList (fun s -> prefix + " " + s + ">" |> next)

    and [<TailCall>] formatVector visited next (xs: SExpression array) =
        if isVisited visited xs then
            "..." |> next
        else
            xs
            |> Array.toList
            |> List.map (fun e -> (xs :> obj) :: visited, e)
            |> formatList (sprintf "#(%s)" >> next)

    and [<TailCall>] formatValues visited next (xs: SExpression list) =
        if isVisited visited xs then
            "..." |> next
        else
            xs
            |> List.map (fun e -> (xs :> obj) :: visited, e)
            |> formatList (fun s -> (if s = "" then "(values)" else sprintf "(values %s)" s) |> next)

    and [<TailCall>] printCPS visited next =
        function
        | SPair p, _ -> p |> formatPair visited next []
        | SVector xs, _ -> xs |> formatVector visited next
        | SValues xs, _ -> xs |> formatValues visited next
        | SError(msg, irritants), _ -> formatError visited next msg irritants
        | x, _ as expr ->
            if isSimpleValueKind x then
                formatSimpleValue expr |> next
            elif isOpaqueDescriptorKind x then
                formatOpaqueDescriptor expr |> next
            elif isQuoteLikeKind x then
                let prefix, inner = getWrapperPrefixAndInner expr
                inner |> printCPS visited (sprintf "%s%s" prefix >> next)
            else
                failwith "unreachable."

    let print x = x |> printCPS [] id
