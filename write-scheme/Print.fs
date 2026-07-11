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
        | SEof, _ -> "#!eof"
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

    let formatPort p =
        let dir =
            match p.direction with
            | Input -> "input"
            | Output -> "output"

        let mode = if p.isTextual then "textual" else "binary"
        let status = if p.isOpen then "open" else "closed"
        $"#<{dir} {mode} port {status}>"

    let formatOpaqueDescriptor =
        function
        | SRecord(_, typeName, _), _ -> $"#<{typeName}>"
        | SDatumRef n, _ -> $"#{n}#"
        | SPromise _, _ -> "#<promise>"
        | SParameter _, _ -> "#<parameter>"
        | SPort p, _ -> formatPort p
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
        | SEof
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
        | SPort _
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

    let tryGetObjRef =
        function
        | SPair p, _ -> Some(p :> obj)
        | SVector xs, _ -> Some(xs :> obj)
        | SValues xs, _ -> Some(xs :> obj)
        | SRecord(_, _, fields), _ -> Some(fields :> obj)
        | SString data, _ -> Some(data :> obj)
        | SByteVector bv, _ -> Some(bv :> obj)
        | SError(_, irritants), _ -> Some(irritants :> obj)
        | _ -> None

    let getExprChildren =
        function
        | SPair p, _ -> [ p.car; p.cdr ]
        | SVector xs, _ -> Array.toList xs
        | SValues xs, _ -> xs
        | SRecord(_, _, fields), _ -> fields |> Array.toList |> List.map (fun f -> f.Value)
        | SError(_, irritants), _ -> irritants
        | SQuote d, _
        | SQuasiquote d, _
        | SUnquote d, _
        | SUnquoteSplicing d, _
        | SDatumLabel(_, d), _ -> [ d ]
        | _ -> []

    [<TailCall>]
    let rec loopCountRefs
        (counts: System.Collections.Generic.Dictionary<obj, int>)
        (traversed: System.Collections.Generic.HashSet<obj>)
        =
        function
        | [] -> counts
        | expr :: rest ->
            match tryGetObjRef expr with
            | Some objRef ->
                if counts.ContainsKey objRef then
                    counts.[objRef] <- counts.[objRef] + 1
                else
                    counts.[objRef] <- 1

                if traversed.Contains objRef then
                    rest |> loopCountRefs counts traversed
                else
                    traversed.Add objRef |> ignore
                    getExprChildren expr @ rest |> loopCountRefs counts traversed
            | None -> getExprChildren expr @ rest |> loopCountRefs counts traversed

    [<TailCall>]
    let rec loopAssign
        (assigned: System.Collections.Generic.HashSet<obj>)
        (shared: System.Collections.Generic.HashSet<obj>)
        (labels: System.Collections.Generic.Dictionary<obj, int>)
        labelNumber
        =
        function
        | [] -> labels
        | expr :: rest ->
            match tryGetObjRef expr with
            | Some objRef ->
                if not (assigned.Contains objRef) then
                    assigned.Add objRef |> ignore

                    let labelNumber' =
                        if shared.Contains objRef then
                            labels.[objRef] <- labelNumber + 1
                            labelNumber + 1
                        else
                            labelNumber

                    getExprChildren expr @ rest |> loopAssign assigned shared labels labelNumber'
                else
                    rest |> loopAssign assigned shared labels labelNumber
            | None -> getExprChildren expr @ rest |> loopAssign assigned shared labels labelNumber

    let buildSharedLabelMap expr =
        let counts = System.Collections.Generic.Dictionary<obj, int>()
        let traversed = System.Collections.Generic.HashSet<obj>()
        let shared = System.Collections.Generic.HashSet<obj>()

        for kvp in [ expr ] |> loopCountRefs counts traversed do
            if kvp.Value > 1 then
                shared.Add kvp.Key |> ignore

        let labels = System.Collections.Generic.Dictionary<obj, int>()
        let assigned = System.Collections.Generic.HashSet<obj>()
        [ expr ] |> loopAssign assigned shared labels 0

    let tryGetLabel (labelMap: System.Collections.Generic.Dictionary<obj, int>) objRef =
        let mutable n = 0
        if labelMap.TryGetValue(objRef, &n) then Some n else None

    let emitLabel emitted labelMap objRef =
        match tryGetLabel labelMap objRef with
        | Some n when not (emitted |> Set.contains n) -> Some(sprintf "#%d=" n, n), emitted |> Set.add n
        | Some n -> Some(sprintf "#%d#" n, n), emitted
        | None -> None, emitted

    let emitLabelDispatch labelMap emitted next objRef withFirst withNone =
        let result, emitted' = emitLabel emitted labelMap objRef

        match result with
        | Some(label, _) when not (label.EndsWith "#") -> withFirst label emitted'
        | Some(label, _) -> next label emitted'
        | None -> withNone () emitted'


    [<TailCall>]
    let rec formatListShared labelMap emitted next =
        function
        | [] -> next "" emitted
        | [ visited, x ] -> x |> printSharedCPS labelMap emitted visited next
        | (visited, x) :: xs ->
            x
            |> printSharedCPS labelMap emitted visited (fun s1 emitted' ->
                xs
                |> formatListShared labelMap emitted' (fun s2 emitted'' -> next (s1 + " " + s2) emitted''))

    and [<TailCall>] formatPairShared labelMap emitted visited outerNext label acc pair =
        if isVisited visited pair then
            match tryGetLabel labelMap (pair :> obj) with
            | Some n when emitted |> Set.contains n -> outerNext $"#{n}#" emitted
            | Some n -> outerNext $"#{n}= ..." (emitted |> Set.add n)
            | _ -> outerNext "..." emitted
        else
            let visited' = (pair :> obj) :: visited

            match pair.cdr with
            | SEmpty, _ ->
                (visited', pair.car) :: acc
                |> List.rev
                |> formatListShared labelMap emitted (fun s emitted' ->
                    let content = $"({s})"

                    match label with
                    | Some l -> outerNext $"{l}{content}" emitted'
                    | None -> outerNext content emitted')
            | SPair p, _ ->
                p
                |> formatPairShared labelMap emitted visited' outerNext label ((visited', pair.car) :: acc)
            | _ ->
                (visited', pair.car) :: acc
                |> List.rev
                |> formatListShared labelMap emitted (fun s1 emitted' ->
                    pair.cdr
                    |> printSharedCPS labelMap emitted' visited' (fun s2 emitted'' ->
                        let content = $"({s1} . {s2})"

                        match label with
                        | Some l -> outerNext $"{l}{content}" emitted''
                        | None -> outerNext content emitted''))

    and [<TailCall>] formatVectorShared labelMap emitted visited outerNext label xs =
        if isVisited visited xs then
            match tryGetLabel labelMap (xs :> obj) with
            | Some n when emitted |> Set.contains n -> outerNext $"#{n}#" emitted
            | Some n -> outerNext $"#{n}= ..." (emitted |> Set.add n)
            | _ -> outerNext "..." emitted
        else
            xs
            |> Array.toList
            |> List.map (fun e -> (xs :> obj) :: visited, e)
            |> formatListShared labelMap emitted (fun s emitted' ->
                let content = $"#({s})"

                match label with
                | Some l -> outerNext $"{l}{content}" emitted'
                | None -> outerNext content emitted')

    and [<TailCall>] formatValuesShared labelMap emitted visited outerNext label xs =
        if isVisited visited xs then
            match tryGetLabel labelMap (xs :> obj) with
            | Some n when emitted |> Set.contains n -> outerNext $"#{n}#" emitted
            | Some n -> outerNext $"#{n}= ..." (emitted |> Set.add n)
            | _ -> outerNext "..." emitted
        else
            xs
            |> List.map (fun e -> (xs :> obj) :: visited, e)
            |> formatListShared labelMap emitted (fun s emitted' ->
                let content = if s = "" then "(values)" else $"(values {s})"

                match label with
                | Some l -> outerNext $"{l}{content}" emitted'
                | None -> outerNext content emitted')

    and [<TailCall>] formatErrorShared labelMap emitted visited outerNext label msg irritants =
        let prefix = msg.runes |> runesToString |> sprintf "#<error \"%s\""

        if isVisited visited irritants then
            match tryGetLabel labelMap (irritants :> obj) with
            | Some n when emitted |> Set.contains n -> outerNext $"{prefix} #{n}#>" emitted
            | Some n -> outerNext $"{prefix} #{n}= ...>" (emitted |> Set.add n)
            | _ -> outerNext $"{prefix} ...>" emitted
        else
            match tryGetLabel labelMap (irritants :> obj) with
            | Some n when emitted |> Set.contains n -> outerNext $"{prefix} #{n}#>" emitted
            | Some n ->
                let visited' = (irritants :> obj) :: visited

                irritants
                |> List.map (fun e -> (irritants :> obj) :: visited', e)
                |> formatListShared labelMap (emitted |> Set.add n) (fun s emitted' ->
                    let content = $"{prefix} {s}>"

                    match label with
                    | Some l -> outerNext $"{l}{content}" emitted'
                    | None -> outerNext content emitted')
            | _ ->
                let visited' = (irritants :> obj) :: visited

                irritants
                |> List.map (fun e -> (irritants :> obj) :: visited', e)
                |> formatListShared labelMap emitted (fun s emitted' ->
                    let content = $"{prefix} {s}>"

                    match label with
                    | Some l -> outerNext $"{l}{content}" emitted'
                    | None -> outerNext content emitted')

    and [<TailCall>] printSharedCPS labelMap emitted visited next =
        function
        | SPair p, _ ->
            emitLabelDispatch
                labelMap
                emitted
                next
                (p :> obj)
                (fun label emitted' -> p |> formatPairShared labelMap emitted' visited next (Some label) [])
                (fun () emitted' -> p |> formatPairShared labelMap emitted' visited next None [])
        | SVector xs, _ ->
            emitLabelDispatch
                labelMap
                emitted
                next
                (xs :> obj)
                (fun label emitted' -> xs |> formatVectorShared labelMap emitted' visited next (Some label))
                (fun () emitted' -> xs |> formatVectorShared labelMap emitted' visited next None)
        | SValues xs, _ ->
            emitLabelDispatch
                labelMap
                emitted
                next
                (xs :> obj)
                (fun label emitted' -> xs |> formatValuesShared labelMap emitted' visited next (Some label))
                (fun () emitted' -> xs |> formatValuesShared labelMap emitted' visited next None)
        | SError(msg, irritants), _ ->
            emitLabelDispatch
                labelMap
                emitted
                next
                (irritants :> obj)
                (fun label emitted' ->
                    let prefix = msg.runes |> runesToString |> sprintf "#<error \"%s\""

                    if isVisited visited irritants then
                        next $"{label}{prefix} ...>" emitted'
                    else
                        irritants
                        |> List.map (fun e -> (irritants :> obj) :: visited, e)
                        |> formatListShared labelMap emitted' (fun s emitted'' ->
                            next $"{label}{prefix} {s}>" emitted''))
                (fun () emitted' -> formatErrorShared labelMap emitted' visited next None msg irritants)
        | x, _ as expr ->
            if isSimpleValueKind x then
                next (formatSimpleValue expr) emitted
            elif isOpaqueDescriptorKind x then
                next (formatOpaqueDescriptor expr) emitted
            elif isQuoteLikeKind x then
                let prefix, inner = getWrapperPrefixAndInner expr

                inner
                |> printSharedCPS labelMap emitted visited (fun s emitted' -> next $"{prefix}{s}" emitted')
            else
                failwith "unreachable."

    let printShared x =
        let labelMap = buildSharedLabelMap x
        x |> printSharedCPS labelMap Set.empty<int> [] (fun s _ -> s)
