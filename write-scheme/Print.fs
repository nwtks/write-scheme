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
            if x >= 0.0 then $"+{x:g}" else $"{x:g}"
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

    let escapeChar c =
        match c with
        | '"' -> "\\\""
        | '\\' -> "\\\\"
        | c -> string c

    let runesToChars (runes: System.Text.Rune array) =
        runes |> Seq.collect (fun r -> (string r).ToCharArray())

    let formatString data =
        let content = data.runes |> runesToChars |> Seq.map escapeChar |> String.concat ""
        $"\"{content}\""

    let namedCharNames =
        Map.ofList
            [ 32, "#\\space"
              10, "#\\newline"
              13, "#\\return"
              9, "#\\tab"
              7, "#\\alarm"
              8, "#\\backspace"
              127, "#\\delete"
              27, "#\\escape"
              0, "#\\null" ]

    let formatChar (c: System.Text.Rune) =
        match namedCharNames |> Map.tryFind c.Value with
        | Some name -> name
        | None when System.Text.Rune.IsControl c -> $"#\\x{c.Value:x}"
        | _ -> c |> string |> sprintf "#\\%s"

    let isInitial c =
        System.Char.IsLetter c || "!$%&*/:<=>?^_~".Contains c

    let isSubsequent c =
        isInitial c || System.Char.IsDigit c || "+-.@".Contains c

    let symbolNeedsPipe s =
        if s = "" || s = "+" || s = "-" || s = "..." then
            false
        else
            not (isInitial s.[0])
            && not (s.[0] = '+' || s.[0] = '-')
            && not (s.Length > 1 && s.[0] = '.' && (isInitial s.[1] || "+-.@".Contains(s.[1])))
            && not (s.StartsWith "...")
            || s |> Seq.exists (not << isSubsequent)

    let formatSymbol s =
        if s = "" then
            "||"
        elif s = "+" || s = "-" || s = "..." then
            s
        elif s |> symbolNeedsPipe then
            "|" + s.Replace("\\", "\\\\").Replace("|", "\\|") + "|"
        else
            s

    let isVisited visited x =
        visited |> List.exists (fun v -> obj.ReferenceEquals(v, x))

    let formatBool b = if b then "#t" else "#f"

    let formatRational n d = if d = 1I then string n else $"{n}/{d}"

    let formatByteVector (xs: byte array) =
        xs |> Array.map string |> String.concat " " |> sprintf "#u8(%s)"

    let formatSimpleValue =
        function
        | SUnspecified, _ -> "#<unspecified>"
        | SEmpty, _ -> "()"
        | SBool b, _ -> formatBool b
        | SRational(n, d), _ -> formatRational n d
        | SReal x, _ -> formatFloat x false
        | SComplex x, _ -> formatComplex x
        | SString data, _ -> formatString data
        | SChar x, _ -> formatChar x
        | SSymbol x, _ -> formatSymbol x
        | SByteVector xs, _ -> formatByteVector xs
        | _ -> failwith "unreachable."

    let formatOpaqueDescriptor =
        function
        | SRecord(_, typeName, _), _ -> $"#<{typeName}>"
        | SDatumRef n, _ -> $"#{n}#"
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
        | SDatumLabel(n, d), _ -> $"#{n}=", d
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
            | _ -> acc |> List.rev |> formatList (fun s -> $"({s} ...)" |> next)
        else
            let visited' = (pair :> obj) :: visited

            match pair.cdr with
            | SEmpty, _ ->
                (visited', pair.car) :: acc
                |> List.rev
                |> formatList (fun s -> $"({s})" |> next)
            | SPair p, _ -> p |> formatPair visited' next ((visited', pair.car) :: acc)
            | _ ->
                (visited', pair.car) :: acc
                |> List.rev
                |> formatList (fun s1 -> pair.cdr |> printCPS visited' (fun s2 -> $"({s1} . {s2})" |> next))

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
            |> formatList (fun s -> $"#({s})" |> next)

    and [<TailCall>] formatValues visited next (xs: SExpression list) =
        if isVisited visited xs then
            "..." |> next
        else
            xs
            |> List.map (fun e -> (xs :> obj) :: visited, e)
            |> formatList (fun s -> (if s = "" then "(values)" else $"(values {s})") |> next)

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
                inner |> printCPS visited (fun s -> $"{prefix}{s}" |> next)
            else
                failwith "unreachable."

    let print x = x |> printCPS [] id
