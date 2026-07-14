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
        elif isImaginary then
            let s = x.ToString("g", System.Globalization.CultureInfo.InvariantCulture)
            if System.Double.IsNegative x then s else "+" + s
        else
            let s = x.ToString("g", System.Globalization.CultureInfo.InvariantCulture)
            if System.Double.IsNegative x && x = 0.0 then "-" + s else s

    let formatComplex (x: System.Numerics.Complex) =
        let real =
            if not (System.Double.IsNegative x.Real) && x.Real = 0.0 && x.Imaginary <> 0.0 then
                "0"
            else
                formatFloat x.Real false

        let imag = formatFloat x.Imaginary true + "i"
        real + imag

    let formatString data =
        let sb = System.Text.StringBuilder "\""

        for r in data.runes do
            for c in (string r).ToCharArray() do
                match c with
                | '"' -> sb.Append "\\\""
                | '\\' -> sb.Append "\\\\"
                | c -> sb.Append c
                |> ignore

        sb.Append "\"" |> ignore
        sb.ToString()

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
        | _ -> $"#\\{c}"

    let isInitial c =
        System.Char.IsLetter c || "!$%&*/:<=>?^_~".Contains c

    let isSubsequent c =
        isInitial c || System.Char.IsDigit c || "+-.@".Contains c

    let symbolNeedsPipe s =
        if s = "" || s = "+" || s = "-" || s = "..." then
            false
        else
            let startsWithInvalid =
                not (isInitial s.[0])
                && not (s.[0] = '+' || s.[0] = '-')
                && not (s.Length > 1 && s.[0] = '.' && (isInitial s.[1] || "+-.@".Contains(s.[1])))
                && not (s.StartsWith "...")

            startsWithInvalid || s |> Seq.exists (not << isSubsequent)

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
        let sb = System.Text.StringBuilder "#u8("

        for i = 0 to xs.Length - 1 do
            if i > 0 then
                sb.Append ' ' |> ignore

            sb.Append(string xs.[i]) |> ignore

        sb.Append ')' |> ignore
        sb.ToString()

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
        | SEnvironment _, _ -> "#<environment>"
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
        | SContinuation _
        | SEnvironment _ -> true
        | _ -> false

    let isQuoteLikeKind =
        function
        | SQuote _
        | SQuasiquote _
        | SUnquote _
        | SUnquoteSplicing _
        | SDatumLabel _ -> true
        | _ -> false

    let tryGetObjRef =
        function
        | SPair p, _ -> Some(p :> obj)
        | SVector xs, _ -> Some(xs :> obj)
        | SValues xs, _ -> Some(xs :> obj)
        | SRecord(_, _, fields), _ -> Some(fields :> obj)
        | SString data, _ -> Some(data :> obj)
        | SByteVector bv, _ -> Some(bv :> obj)
        | SError(_, _, irritants), _ -> Some(irritants :> obj)
        | _ -> None

    let getExprChildren =
        function
        | SPair p, _ -> [ p.car; p.cdr ]
        | SVector xs, _ -> Array.toList xs
        | SValues xs, _ -> xs
        | SRecord(_, _, fields), _ -> fields |> Array.toList |> List.map (fun f -> f.Value)
        | SError(_, _, irritants), _ -> irritants
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
        | Some n when not (emitted |> Set.contains n) -> Some(sprintf "#%d=" n), emitted |> Set.add n
        | Some n -> Some(sprintf "#%d#" n), emitted
        | None -> None, emitted

    let emitLabelDispatch labelMap emitted next objRef withFirst withNone =
        let result, emitted' = emitLabel emitted labelMap objRef

        match result with
        | Some label when not (label.EndsWith "#") -> withFirst label emitted'
        | Some label -> next label emitted'
        | None -> withNone () emitted'

    let withLabel label outerNext emitted' s =
        match label with
        | Some l -> outerNext $"{l}{s}" emitted'
        | None -> outerNext s emitted'

    [<TailCall>]
    let rec formatList labelMap emitted next =
        function
        | [] -> next "" emitted
        | [ visited, x ] -> x |> printCPS labelMap emitted visited next
        | (visited, x) :: xs ->
            x
            |> printCPS labelMap emitted visited (fun s1 emitted' ->
                xs
                |> formatList labelMap emitted' (fun s2 emitted'' -> next (s1 + " " + s2) emitted''))

    and [<TailCall>] formatPair labelMap emitted visited outerNext label acc pair =
        if isVisited visited pair then
            match tryGetLabel labelMap (pair :> obj) with
            | Some n when emitted |> Set.contains n -> outerNext $"#{n}#" emitted
            | Some n -> outerNext $"#{n}= ..." (emitted |> Set.add n)
            | _ ->
                match acc with
                | [] -> outerNext "..." emitted
                | _ ->
                    acc
                    |> List.rev
                    |> formatList labelMap emitted (fun s emitted' -> outerNext $"({s} ...)" emitted')
        else
            let visited' = (pair :> obj) :: visited

            match pair.cdr with
            | SEmpty, _ ->
                (visited', pair.car) :: acc
                |> List.rev
                |> formatList labelMap emitted (fun s emitted' -> $"({s})" |> withLabel label outerNext emitted')
            | SPair p, _ ->
                p
                |> formatPair labelMap emitted visited' outerNext label ((visited', pair.car) :: acc)
            | _ ->
                (visited', pair.car) :: acc
                |> List.rev
                |> formatList labelMap emitted (fun s1 emitted' ->
                    pair.cdr
                    |> printCPS labelMap emitted' visited' (fun s2 emitted'' ->
                        $"({s1} . {s2})" |> withLabel label outerNext emitted''))

    and [<TailCall>] formatVector labelMap emitted visited outerNext label xs =
        if isVisited visited xs then
            match tryGetLabel labelMap (xs :> obj) with
            | Some n when emitted |> Set.contains n -> outerNext $"#{n}#" emitted
            | Some n -> outerNext $"#{n}= ..." (emitted |> Set.add n)
            | _ -> outerNext "..." emitted
        else
            xs
            |> Array.toList
            |> List.map (fun e -> (xs :> obj) :: visited, e)
            |> formatList labelMap emitted (fun s emitted' -> $"#({s})" |> withLabel label outerNext emitted')

    and [<TailCall>] formatValues labelMap emitted visited outerNext label xs =
        if isVisited visited xs then
            match tryGetLabel labelMap (xs :> obj) with
            | Some n when emitted |> Set.contains n -> outerNext $"#{n}#" emitted
            | Some n -> outerNext $"#{n}= ..." (emitted |> Set.add n)
            | _ -> outerNext "..." emitted
        else
            xs
            |> List.map (fun e -> (xs :> obj) :: visited, e)
            |> formatList labelMap emitted (fun s emitted' ->
                (if s = "" then "(values)" else $"(values {s})")
                |> withLabel label outerNext emitted')

    and [<TailCall>] formatError labelMap emitted visited outerNext label msg irritants =
        let prefix = msg.runes |> runesToString |> sprintf "#<error \"%s\""

        if isVisited visited irritants then
            $"{prefix} ...>" |> withLabel label outerNext emitted
        else
            let visited' = (irritants :> obj) :: visited

            irritants
            |> List.map (fun e -> (irritants :> obj) :: visited', e)
            |> formatList labelMap emitted (fun s emitted' -> $"{prefix} {s}>" |> withLabel label outerNext emitted')

    and [<TailCall>] printCPS labelMap emitted visited next =
        function
        | SPair p, _ ->
            emitLabelDispatch
                labelMap
                emitted
                next
                (p :> obj)
                (fun label emitted' -> p |> formatPair labelMap emitted' visited next (Some label) [])
                (fun () emitted' -> p |> formatPair labelMap emitted' visited next None [])
        | SVector xs, _ ->
            emitLabelDispatch
                labelMap
                emitted
                next
                (xs :> obj)
                (fun label emitted' -> xs |> formatVector labelMap emitted' visited next (Some label))
                (fun () emitted' -> xs |> formatVector labelMap emitted' visited next None)
        | SValues xs, _ ->
            emitLabelDispatch
                labelMap
                emitted
                next
                (xs :> obj)
                (fun label emitted' -> xs |> formatValues labelMap emitted' visited next (Some label))
                (fun () emitted' -> xs |> formatValues labelMap emitted' visited next None)
        | SError(_, msg, irritants), _ ->
            emitLabelDispatch
                labelMap
                emitted
                next
                (irritants :> obj)
                (fun label emitted' -> formatError labelMap emitted' visited next (Some label) msg irritants)
                (fun () emitted' -> formatError labelMap emitted' visited next None msg irritants)
        | x, _ as expr ->
            if isSimpleValueKind x then
                next (formatSimpleValue expr) emitted
            elif isOpaqueDescriptorKind x then
                next (formatOpaqueDescriptor expr) emitted
            elif isQuoteLikeKind x then
                let prefix, inner = getWrapperPrefixAndInner expr

                inner
                |> printCPS labelMap emitted visited (fun s emitted' -> next $"{prefix}{s}" emitted')
            else
                failwith "unreachable."

    let print x =
        x
        |> printCPS (System.Collections.Generic.Dictionary<obj, int>()) Set.empty<int> [] (fun s _ -> s)

    let printShared x =
        let labelMap = buildSharedLabelMap x
        x |> printCPS labelMap Set.empty<int> [] (fun s _ -> s)
