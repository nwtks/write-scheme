namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Macro =
    type SBinding =
        | SingleB of SExpression
        | EllipsisB of SBinding list

    let mergeBindings bindings1 bindings2 =
        Map.fold (fun acc k v -> acc |> Map.add k v) bindings1 bindings2

    [<TailCall>]
    let rec decodePair acc =
        function
        | SPair p, _ -> p.cdr |> decodePair (p.car :: acc)
        | x -> acc |> List.rev, x

    [<TailCall>]
    let rec loopPatternVars literals ellipsis acc =
        function
        | [] -> acc
        | x :: xs ->
            match x with
            | SSymbol "_", _ -> loopPatternVars literals ellipsis acc xs
            | SSymbol s, _ when s = ellipsis -> loopPatternVars literals ellipsis acc xs
            | SSymbol s, _ when literals |> Set.contains s -> loopPatternVars literals ellipsis acc xs
            | SSymbol s, _ -> loopPatternVars literals ellipsis (s :: acc) xs
            | SPair _, _ as pair ->
                let elements, tail = pair |> decodePair []
                loopPatternVars literals ellipsis acc (elements @ tail :: xs)
            | SVector pats, _ -> loopPatternVars literals ellipsis acc (Array.toList pats @ xs)
            | _ -> loopPatternVars literals ellipsis acc xs

    let collectPatternVariables literals ellipsis pattern =
        [ pattern ] |> loopPatternVars literals ellipsis [] |> List.distinct |> List.rev

    let freeIdentifierEquals defContext defId useContext useId =
        match defId, useId with
        | SSymbol defSym, SSymbol useSym ->
            let defRef = defSym |> Context.tryLookupEnvironments defContext
            let useRef = useSym |> Context.tryLookupEnvironments useContext

            match defRef, useRef with
            | Some defVal, Some useVal -> LanguagePrimitives.PhysicalEquality defVal useVal
            | None, None -> defSym = useSym
            | _ -> false
        | _ -> false

    let buildEllipsisBindings variables bindings =
        variables
        |> List.fold
            (fun acc variable ->
                let values =
                    bindings
                    |> List.map (fun binding ->
                        match binding |> Map.tryFind variable with
                        | Some b -> b
                        | None -> SingleB(SEmpty, None))

                acc |> Map.add variable (EllipsisB values))
            Map.empty

    [<TailCall>]
    let rec matchOne defContext useContext literals ellipsis arg next =
        function
        | SSymbol "_", _ -> Map.empty |> Some |> next
        | SSymbol s, _ when s = ellipsis -> None |> next
        | SSymbol s as sym, _ when literals |> Set.contains s ->
            if freeIdentifierEquals defContext sym useContext (fst arg) then
                Map.empty |> Some |> next
            else
                None |> next
        | SSymbol s, _ -> Map.ofList [ s, SingleB arg ] |> Some |> next
        | SEmpty, _ ->
            match arg with
            | SEmpty, _ -> Map.empty |> Some |> next
            | _ -> None |> next
        | SBool v, _ ->
            match arg with
            | SBool v', _ when v = v' -> Map.empty |> Some |> next
            | _ -> None |> next
        | SRational(n1, d1), _ ->
            match arg with
            | SRational(n2, d2), _ when n1 = n2 && d1 = d2 -> Map.empty |> Some |> next
            | _ -> None |> next
        | SReal v, _ ->
            match arg with
            | SReal v', _ when v = v' -> Map.empty |> Some |> next
            | _ -> None |> next
        | SString v, _ ->
            match arg with
            | SString v', _ when v = v' -> Map.empty |> Some |> next
            | _ -> None |> next
        | SChar v, _ ->
            match arg with
            | SChar v', _ when v = v' -> Map.empty |> Some |> next
            | _ -> None |> next
        | SPair { car = SSymbol ell, _
                  cdr = SPair { car = SSymbol s, _; cdr = SEmpty, _ }, _ },
          _ when ell = ellipsis && s = ellipsis ->
            match arg with
            | SSymbol s', _ when s' = ellipsis -> Map.empty |> Some |> next
            | _ -> None |> next
        | SPair _, _ as pair -> pair |> matchOnePair defContext useContext literals ellipsis arg next
        | SVector patterns, _ ->
            match arg with
            | SVector args, _ when patterns.Length = args.Length ->
                patterns
                |> Array.toList
                |> matchPatternList defContext useContext literals ellipsis (args |> Array.toList) next
            | _ -> None |> next
        | _ -> None |> next

    and [<TailCall>] matchOnePair defContext useContext literals ellipsis arg next pair =
        let patterns, tail = pair |> decodePair []
        let isProper = fst tail = SEmpty

        match arg with
        | SPair _, _ as a when isProper && a |> isProperList ->
            match a |> toList with
            | Ok args -> patterns |> matchPatternList defContext useContext literals ellipsis args next
            | Error _ -> None |> next
        | SPair _, _ as a when not isProper ->
            (pair, a) |> loopMatchOnePair defContext useContext literals ellipsis next
        | _ -> None |> next

    and [<TailCall>] loopMatchOnePair defContext useContext literals ellipsis next =
        function
        | (SPair pattern, _), (SPair arg, _) ->
            pattern.car
            |> matchOne defContext useContext literals ellipsis arg.car (function
                | Some binding1 ->
                    (pattern.cdr, arg.cdr)
                    |> loopMatchOnePair defContext useContext literals ellipsis (function
                        | Some binding2 -> mergeBindings binding1 binding2 |> Some |> next
                        | None -> None |> next)
                | None -> None |> next)
        | pattern, arg -> pattern |> matchOne defContext useContext literals ellipsis arg next

    and [<TailCall>] matchPatternList defContext useContext literals ellipsis args next patterns =
        let dotIdx =
            patterns
            |> List.tryFindIndex (function
                | SSymbol s, _ when s = ellipsis -> true
                | _ -> false)

        match dotIdx with
        | Some i when i > 0 ->
            let prefixPatterns = patterns |> List.take (i - 1)
            let suffixPatterns = patterns |> List.skip (i + 1)
            let ellipsisPattern = patterns.[i - 1]

            if args.Length < prefixPatterns.Length + suffixPatterns.Length then
                None |> next
            else
                matchPatternListWithEllipsisParts
                    defContext
                    useContext
                    literals
                    ellipsis
                    args
                    next
                    prefixPatterns
                    suffixPatterns
                    ellipsisPattern
        | _ ->
            match patterns with
            | pattern :: rest -> matchPatternListCons defContext useContext literals ellipsis args next pattern rest
            | [] ->
                if args |> List.isEmpty then Map.empty |> Some else None
                |> next

    and [<TailCall>] matchPatternListWithEllipsisParts
        defContext
        useContext
        literals
        ellipsis
        args
        next
        prefixPatterns
        suffixPatterns
        ellipsisPattern
        =
        let prefixArgs = args |> List.take prefixPatterns.Length
        let suffixArgs = args |> List.skip (args.Length - suffixPatterns.Length)

        let ellipsisArgs =
            args
            |> List.skip prefixPatterns.Length
            |> List.take (args.Length - prefixPatterns.Length - suffixPatterns.Length)

        prefixPatterns
        |> matchPatternList defContext useContext literals ellipsis prefixArgs (function
            | Some matchedPrefix ->
                suffixPatterns
                |> matchPatternList defContext useContext literals ellipsis suffixArgs (function
                    | Some matchedSuffix ->
                        let variables = ellipsisPattern |> collectPatternVariables literals ellipsis

                        ellipsisArgs
                        |> matchEllipsis defContext useContext literals ellipsis ellipsisPattern variables [] (function
                            | Some matchedEllipsis ->
                                mergeBindings (mergeBindings matchedPrefix matchedSuffix) matchedEllipsis
                                |> Some
                                |> next
                            | None -> None |> next)
                    | None -> None |> next)
            | None -> None |> next)

    and [<TailCall>] matchPatternListCons defContext useContext literals ellipsis args next pattern rest =
        match args with
        | arg :: restArgs ->
            pattern
            |> matchOne defContext useContext literals ellipsis arg (function
                | Some binding1 ->
                    rest
                    |> matchPatternList defContext useContext literals ellipsis restArgs (fun matchedRest ->
                        match matchedRest with
                        | Some binding2 -> mergeBindings binding1 binding2 |> Some |> next
                        | None -> None |> next)
                | None -> None |> next)
        | [] -> None |> next

    and [<TailCall>] matchEllipsis defContext useContext literals ellipsis pattern variables results next =
        function
        | [] ->
            results
            |> List.rev
            |> List.map Option.get
            |> buildEllipsisBindings variables
            |> Some
            |> next
        | inp :: restInps ->
            pattern
            |> matchOne defContext useContext literals ellipsis inp (function
                | Some binding ->
                    restInps
                    |> matchEllipsis
                        defContext
                        useContext
                        literals
                        ellipsis
                        pattern
                        variables
                        (Some binding :: results)
                        next
                | None -> None |> next)

    [<TailCall>]
    let rec loopTemplateVars ellipsis acc =
        function
        | [] -> acc
        | x :: xs ->
            match x with
            | SSymbol v, _ -> xs |> loopTemplateVars ellipsis (v :: acc)
            | SPair _, _ as pair ->
                let elements, tail = pair |> decodePair []
                elements @ tail :: xs |> loopTemplateVars ellipsis acc
            | SQuote v, _ -> v :: xs |> loopTemplateVars ellipsis acc
            | SQuasiquote v, _ -> v :: xs |> loopTemplateVars ellipsis acc
            | SUnquote v, _ -> v :: xs |> loopTemplateVars ellipsis acc
            | SUnquoteSplicing v, _ -> v :: xs |> loopTemplateVars ellipsis acc
            | SVector v, _ -> (v |> Array.toList) @ xs |> loopTemplateVars ellipsis acc
            | _ -> xs |> loopTemplateVars ellipsis acc

    let collectTemplateVars ellipsis template =
        [ template ] |> loopTemplateVars ellipsis [] |> List.distinct |> List.rev

    [<TailCall>]
    let rec renameTemplate toRename next =
        function
        | SSymbol var, pos as sym ->
            match toRename |> Map.tryFind var with
            | Some s -> (SSymbol s, pos) |> next
            | None -> sym |> next
        | SPair _, _ as pair -> pair |> renameTemplatePair toRename next
        | SVector x, pos ->
            x
            |> Array.toList
            |> renameTemplateList toRename (List.toArray >> SVector >> (fun x -> x, pos) >> next)
        | SQuote x, pos -> x |> renameTemplate toRename (SQuote >> (fun x -> x, pos) >> next)
        | SQuasiquote x, pos -> x |> renameTemplate toRename (SQuasiquote >> (fun x -> x, pos) >> next)
        | SUnquote x, pos -> x |> renameTemplate toRename (SUnquote >> (fun x -> x, pos) >> next)
        | SUnquoteSplicing x, pos -> x |> renameTemplate toRename (SUnquoteSplicing >> (fun x -> x, pos) >> next)
        | x -> x |> next

    and [<TailCall>] renameTemplateList toRename next =
        function
        | [] -> [] |> next
        | x :: xs ->
            x
            |> renameTemplate toRename (fun renamedX ->
                xs
                |> renameTemplateList toRename (fun renamedXs -> renamedX :: renamedXs |> next))

    and [<TailCall>] renameTemplatePair toRename next pair =
        let elements, tail = pair |> decodePair []
        let isProper = fst tail = SEmpty

        elements
        |> renameTemplateList toRename (fun renamedElements ->
            if isProper then
                renamedElements |> toSPair |> next
            else
                tail
                |> renameTemplate
                    toRename
                    (List.foldBack (fun x acc -> SPair { car = x; cdr = acc }, snd x) renamedElements
                     >> next))

    [<TailCall>]
    let rec expandTemplate ellipsis isRaw bindings next =
        function
        | SSymbol var, _ as sym ->
            match bindings |> Map.tryFind var with
            | Some(SingleB v) -> v |> next
            | _ -> sym |> next
        | SPair { car = SSymbol ell, _
                  cdr = SPair { car = template; cdr = SEmpty, _ }, _ },
          _ when not isRaw && ell = ellipsis -> template |> expandTemplate ellipsis true bindings next
        | SPair _, _ as pair -> pair |> expandTemplatePair ellipsis isRaw bindings next
        | SQuote x, pos ->
            x
            |> expandTemplate ellipsis isRaw bindings (SQuote >> (fun x -> x, pos) >> next)
        | SQuasiquote x, pos ->
            x
            |> expandTemplate ellipsis isRaw bindings (SQuasiquote >> (fun x -> x, pos) >> next)
        | SUnquote x, pos ->
            x
            |> expandTemplate ellipsis isRaw bindings (SUnquote >> (fun x -> x, pos) >> next)
        | SUnquoteSplicing x, pos ->
            x
            |> expandTemplate ellipsis isRaw bindings (SUnquoteSplicing >> (fun x -> x, pos) >> next)
        | SVector x, pos ->
            x
            |> Array.toList
            |> expandTemplateList ellipsis isRaw bindings (List.toArray >> SVector >> (fun x -> x, pos) >> next)
        | x -> x |> next

    and [<TailCall>] expandTemplateList ellipsis isRaw bindings next =
        function
        | [] -> [] |> next
        | template :: (SSymbol ell, _) :: rest when not isRaw && ell = ellipsis ->
            expandTemplateListWithEllipsis ellipsis isRaw bindings next template rest
        | template :: rest ->
            template
            |> expandTemplate ellipsis isRaw bindings (fun expandedTemplate ->
                rest
                |> expandTemplateList ellipsis isRaw bindings (fun expandedRest ->
                    expandedTemplate :: expandedRest |> next))

    and [<TailCall>] expandTemplateListWithEllipsis ellipsis isRaw bindings next template rest =
        let ellipsisVars =
            template
            |> collectTemplateVars ellipsis
            |> List.choose (fun v ->
                match bindings |> Map.tryFind v with
                | Some(EllipsisB values) -> Some(v, values)
                | _ -> None)

        match ellipsisVars with
        | [] -> rest |> expandTemplateList ellipsis isRaw bindings next
        | (_, firstValues) :: _ ->
            expandEllipsis
                ellipsis
                bindings
                template
                ellipsisVars
                firstValues.Length
                0
                (fun expanded ->
                    rest
                    |> expandTemplateList ellipsis isRaw bindings (fun expandedRest -> expanded @ expandedRest |> next))
                []

    and [<TailCall>] expandEllipsis ellipsis bindings template ellipsisVars count i next acc =
        if i >= count then
            acc |> List.rev |> next
        else
            let localBindings =
                ellipsisVars
                |> List.fold (fun acc (v, values) -> acc |> Map.add v values.[i]) bindings

            template
            |> expandTemplate ellipsis false localBindings (fun expandedTemplate ->
                expandedTemplate :: acc
                |> expandEllipsis ellipsis bindings template ellipsisVars count (i + 1) next)

    and [<TailCall>] expandTemplatePair ellipsis isRaw bindings next pair =
        let elements, tail = pair |> decodePair []
        let isProper = fst tail = SEmpty

        elements
        |> expandTemplateList ellipsis isRaw bindings (fun expandedElements ->
            if isProper then
                expandedElements |> toSPair |> next
            else
                tail
                |> expandTemplate
                    ellipsis
                    isRaw
                    bindings
                    (List.foldBack (fun x acc -> SPair { car = x; cdr = acc }, snd x) expandedElements
                     >> next))

    let expandSyntaxRule defContext useContext literalSet ellipsis cont elements template bindings =
        let patternVars =
            elements |> toSPair |> collectPatternVariables literalSet ellipsis |> Set.ofList

        let templateVars =
            template
            |> collectTemplateVars ellipsis
            |> List.filter (fun s ->
                not (patternVars |> Set.contains s || literalSet |> Set.contains s || s = ellipsis))
            |> List.distinct

        let expansionId = Context.getNextExpansionId useContext
        let rename s = sprintf "%s#%d" s expansionId
        let renameMap = templateVars |> List.map (fun s -> s, rename s) |> Map.ofList

        template
        |> renameTemplate renameMap (fun renamedTemplate ->
            let extendedContext =
                templateVars
                |> List.choose (fun s ->
                    match s |> Context.tryLookupEnvironments defContext with
                    | Some v -> Some(rename s, v)
                    | None -> None)
                |> Context.extendEnvironments useContext

            renamedTemplate
            |> expandTemplate ellipsis false bindings (Eval.eval extendedContext cont))

    [<TailCall>]
    let rec trySyntaxRules defContext useContext pos cont ellipsis literalSet args =
        function
        | [] -> EvalError("No matching syntax-rules pattern.", pos) |> Error |> cont
        | (elements, template) :: rest ->
            elements
            |> matchPatternList defContext useContext literalSet ellipsis args (function
                | Some bindings ->
                    expandSyntaxRule defContext useContext literalSet ellipsis cont elements template bindings
                | None -> rest |> trySyntaxRules defContext useContext pos cont ellipsis literalSet args)

    let parseSyntaxLiterals =
        function
        | SEmpty, _ -> Ok Set.empty
        | literals when literals |> isProperList ->
            match literals |> toList with
            | Ok l ->
                l
                |> List.choose (function
                    | SSymbol s, _ -> Some s
                    | _ -> None)
                |> Set.ofList
                |> Ok
            | Error e -> Error e
        | x -> x |> invalid (snd x) "'%s' invalid syntax-rules literals."

    let parseSyntaxRule =
        function
        | SPair { car = SPair { car = _; cdr = elements }, _
                  cdr = SPair { car = template; cdr = SEmpty, _ }, _ },
          _ -> elements |> toList |> Result.map (fun elist -> elist, template)
        | x -> x |> invalid (snd x) "'%s' invalid syntax-rules clause."

    let sSyntaxRules context pos cont =
        function
        | (SSymbol ellipsis, _) :: literals :: rules ->
            match parseSyntaxLiterals literals with
            | Ok literalSet ->
                match rules |> mapResult parseSyntaxRule with
                | Ok parsedRules ->
                    let transformer context' pos' cont' args =
                        parsedRules
                        |> trySyntaxRules context context' pos' cont' ellipsis literalSet args

                    Ok(SSyntax transformer, pos) |> cont
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | literals :: rules ->
            match parseSyntaxLiterals literals with
            | Ok literalSet ->
                match rules |> mapResult parseSyntaxRule with
                | Ok parsedRules ->
                    let transformer context' pos' cont' args =
                        parsedRules |> trySyntaxRules context context' pos' cont' "..." literalSet args

                    Ok(SSyntax transformer, pos) |> cont
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid syntax-rules parameter." |> cont

    [<TailCall>]
    let rec evalLetSyntaxTransformers context pos cont body acc =
        function
        | [] ->
            body
            |> Eval.eachEval (acc |> Context.extendEnvironments context) cont (Ok(SEmpty, pos))
        | (var, expr) :: rest ->
            expr
            |> Eval.eval context (function
                | Ok transformer ->
                    rest
                    |> evalLetSyntaxTransformers context pos cont body ((var, ref transformer) :: acc)
                | x -> x |> cont)

    let sLetSyntax context pos cont =
        function
        | bindings :: body ->
            match bindings |> toList with
            | Ok blist ->
                match blist |> mapResult eachBinding with
                | Ok bindings' -> bindings' |> evalLetSyntaxTransformers context pos cont body []
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid let-syntax parameter." |> cont

    [<TailCall>]
    let rec evalLetRecSyntaxTransformers context pos cont body =
        function
        | [], _
        | _, [] -> body |> Eval.eachEval context cont (Ok(SEmpty, pos))
        | (_, expr) :: rest, r: SExpression ref :: restRefs ->
            expr
            |> Eval.eval context (function
                | Ok transformer ->
                    r.Value <- transformer
                    (rest, restRefs) |> evalLetRecSyntaxTransformers context pos cont body
                | x -> x |> cont)

    let sLetRecSyntax context pos cont =
        function
        | bindings :: body ->
            match bindings |> toList with
            | Ok blist ->
                match blist |> mapResult eachBinding with
                | Ok bindings' ->
                    let vars = bindings' |> List.map (fun (v, _) -> v, ref (SEmpty, pos))
                    let context' = vars |> Context.extendEnvironments context

                    (bindings', vars |> List.map snd)
                    |> evalLetRecSyntaxTransformers context' pos cont body
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid letrec-syntax parameter." |> cont

    let sDefineSyntax context pos cont =
        function
        | [ SSymbol keyword, _ as sym; transformer ] ->
            transformer
            |> Eval.eval
                context
                (Result.map (fun x ->
                    Context.defineEnvironmentVariable context keyword x
                    sym)
                 >> cont)
        | x -> x |> invalidParameter pos "'%s' invalid define-syntax parameter." |> cont

    let sSyntaxError context pos cont =
        function
        | (SString message, _) :: irritants -> Error(SchemeRaise((SError(message, irritants), pos), pos)) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid syntax-error parameter." |> cont
