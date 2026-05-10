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

    let collectPatternVars literals ellipsis pattern =
        [ pattern ] |> loopPatternVars literals ellipsis [] |> List.distinct |> List.rev

    let freeIdentifierEquals defEnvs id1 useEnvs id2 =
        match id1, id2 with
        | SSymbol s1, SSymbol s2 ->
            let ref1 = Context.tryLookupEnvs defEnvs s1
            let ref2 = Context.tryLookupEnvs useEnvs s2

            match ref1, ref2 with
            | Some r1, Some r2 -> LanguagePrimitives.PhysicalEquality r1 r2
            | None, None -> s1 = s2
            | _ -> false
        | _ -> false

    [<TailCall>]
    let rec matchOne defEnvs useEnvs literals ellipsis inp next =
        function
        | SSymbol "_", _ -> Map.empty |> Some |> next
        | SSymbol s, _ when s = ellipsis -> None |> next
        | SSymbol s as sym, _ when literals |> Set.contains s ->
            if freeIdentifierEquals defEnvs sym useEnvs (fst inp) then
                Map.empty |> Some |> next
            else
                None |> next
        | SSymbol s, _ -> Map.ofList [ s, SingleB inp ] |> Some |> next
        | SEmpty, _ ->
            match inp with
            | SEmpty, _ -> Map.empty |> Some |> next
            | _ -> None |> next
        | SBool v, _ ->
            match inp with
            | SBool v', _ when v = v' -> Map.empty |> Some |> next
            | _ -> None |> next
        | SRational(n1, d1), _ ->
            match inp with
            | SRational(n2, d2), _ when n1 = n2 && d1 = d2 -> Map.empty |> Some |> next
            | _ -> None |> next
        | SReal v, _ ->
            match inp with
            | SReal v', _ when v = v' -> Map.empty |> Some |> next
            | _ -> None |> next
        | SString v, _ ->
            match inp with
            | SString v', _ when v = v' -> Map.empty |> Some |> next
            | _ -> None |> next
        | SChar v, _ ->
            match inp with
            | SChar v', _ when v = v' -> Map.empty |> Some |> next
            | _ -> None |> next
        | SPair { car = SSymbol ell, _
                  cdr = SPair { car = SSymbol s, _; cdr = SEmpty, _ }, _ },
          _ when ell = ellipsis && s = ellipsis ->
            match inp with
            | SSymbol s', _ when s' = ellipsis -> Map.empty |> Some |> next
            | _ -> None |> next
        | SPair _, _ as pair ->
            let elements, tail = pair |> decodePair []
            let isProper = fst tail = SEmpty

            match inp with
            | SPair _, _ as i when isProper && isProperList i ->
                match i |> toList with
                | Ok ilist -> elements |> matchPatternList defEnvs useEnvs literals ellipsis ilist next
                | Error _ -> None |> next
            | SPair _, _ as i when not isProper -> (pair, i) |> loopMatchOnePair defEnvs useEnvs literals ellipsis next
            | _ -> None |> next
        | SVector patterns, _ ->
            match inp with
            | SVector inps, _ when patterns.Length = inps.Length ->
                patterns
                |> Array.toList
                |> matchPatternList defEnvs useEnvs literals ellipsis (inps |> Array.toList) next
            | _ -> None |> next
        | _ -> None |> next

    and [<TailCall>] loopMatchOnePair defEnvs useEnvs literals ellipsis next =
        function
        | (SPair pair, _), (SPair inp, _) ->
            pair.car
            |> matchOne defEnvs useEnvs literals ellipsis inp.car (function
                | Some binding1 ->
                    (pair.cdr, inp.cdr)
                    |> loopMatchOnePair defEnvs useEnvs literals ellipsis (function
                        | Some binding2 -> mergeBindings binding1 binding2 |> Some |> next
                        | None -> None |> next)
                | None -> None |> next)
        | pattern, inp -> pattern |> matchOne defEnvs useEnvs literals ellipsis inp next

    and [<TailCall>] matchPatternList defEnvs useEnvs literals ellipsis inps next patterns =
        let dotIdx =
            patterns
            |> List.tryFindIndex (function
                | SSymbol s, _ when s = ellipsis -> true
                | _ -> false)

        match dotIdx with
        | Some i when i > 0 ->
            let prefix = patterns |> List.take (i - 1)
            let suffix = patterns |> List.skip (i + 1)

            if inps.Length < prefix.Length + suffix.Length then
                None |> next
            else
                let ellipsisPattern = patterns.[i - 1]
                let prefixInps = inps |> List.take prefix.Length
                let suffixInps = inps |> List.skip (inps.Length - suffix.Length)

                let ellipsisInps =
                    inps
                    |> List.skip prefix.Length
                    |> List.take (inps.Length - prefix.Length - suffix.Length)

                prefix
                |> matchPatternList defEnvs useEnvs literals ellipsis prefixInps (function
                    | Some matchedPrefix ->
                        suffix
                        |> matchPatternList defEnvs useEnvs literals ellipsis suffixInps (function
                            | Some matchedSuffix ->
                                let vars = ellipsisPattern |> collectPatternVars literals ellipsis

                                ellipsisInps
                                |> matchEllipsis defEnvs useEnvs literals ellipsis ellipsisPattern vars [] (function
                                    | Some matchedEllipsis ->
                                        mergeBindings (mergeBindings matchedPrefix matchedSuffix) matchedEllipsis
                                        |> Some
                                        |> next
                                    | None -> None |> next)
                            | None -> None |> next)
                    | None -> None |> next)
        | _ ->
            match patterns with
            | pattern :: rest ->
                match inps with
                | inp :: restInps ->
                    pattern
                    |> matchOne defEnvs useEnvs literals ellipsis inp (function
                        | Some binding1 ->
                            rest
                            |> matchPatternList defEnvs useEnvs literals ellipsis restInps (fun matchedRest ->
                                match matchedRest with
                                | Some binding2 -> mergeBindings binding1 binding2 |> Some |> next
                                | None -> None |> next)
                        | None -> None |> next)
                | [] -> None |> next
            | [] ->
                if List.isEmpty inps then
                    Map.empty |> Some |> next
                else
                    None |> next

    and [<TailCall>] matchEllipsis defEnvs useEnvs literals ellipsis pattern vars results next =
        function
        | [] ->
            let bindings = results |> List.rev |> List.map Option.get

            vars
            |> List.fold
                (fun acc var ->
                    let values =
                        bindings
                        |> List.map (fun binding ->
                            match binding |> Map.tryFind var with
                            | Some b -> b
                            | None -> SingleB(SEmpty, None))

                    acc |> Map.add var (EllipsisB values))
                Map.empty
            |> Some
            |> next
        | inp :: restInps ->
            pattern
            |> matchOne defEnvs useEnvs literals ellipsis inp (function
                | Some binding ->
                    restInps
                    |> matchEllipsis defEnvs useEnvs literals ellipsis pattern vars (Some binding :: results) next
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
        | SPair _, _ as pair ->
            let elements, tail = pair |> decodePair []
            let isProper = fst tail = SEmpty

            elements
            |> renameTemplateList toRename (fun renamedElements ->
                if isProper then
                    toSPair renamedElements |> next
                else
                    tail
                    |> renameTemplate
                        toRename
                        (List.foldBack (fun x acc -> SPair { car = x; cdr = acc }, snd x) renamedElements
                         >> next))
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
        | SPair _, _ as pair ->
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
                let count = firstValues.Length

                expandEllipsis
                    ellipsis
                    bindings
                    template
                    ellipsisVars
                    count
                    0
                    (fun expanded ->
                        rest
                        |> expandTemplateList ellipsis isRaw bindings (fun expandedRest ->
                            expanded @ expandedRest |> next))
                    []
        | template :: rest ->
            template
            |> expandTemplate ellipsis isRaw bindings (fun expandedTemplate ->
                rest
                |> expandTemplateList ellipsis isRaw bindings (fun expandedRest ->
                    expandedTemplate :: expandedRest |> next))

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

    [<TailCall>]
    let rec trySyntaxRules defEnvs useEnvs pos cont ellipsis literalSet args =
        function
        | [] -> EvalError("No matching syntax-rules pattern.", pos) |> Error |> cont
        | (elements, template) :: rest ->
            elements
            |> matchPatternList defEnvs useEnvs literalSet ellipsis args (function
                | Some bindings ->
                    let patternVars =
                        elements |> toSPair |> collectPatternVars literalSet ellipsis |> Set.ofList

                    let templateVars =
                        template
                        |> collectTemplateVars ellipsis
                        |> List.filter (fun s ->
                            not (patternVars |> Set.contains s || literalSet |> Set.contains s || s = ellipsis))
                        |> List.distinct

                    let expansionId = Context.getNextExpansionId useEnvs
                    let rename s = sprintf "%s#%d" s expansionId
                    let renameMap = templateVars |> List.map (fun s -> s, rename s) |> Map.ofList

                    template
                    |> renameTemplate renameMap (fun renamedTemplate ->
                        let extendedEnvs =
                            templateVars
                            |> List.choose (fun s ->
                                match Context.tryLookupEnvs defEnvs s with
                                | Some v -> Some(rename s, v)
                                | None -> None)
                            |> Context.extendEnvs useEnvs

                        renamedTemplate
                        |> expandTemplate ellipsis false bindings (Eval.eval extendedEnvs cont))
                | None -> rest |> trySyntaxRules defEnvs useEnvs pos cont ellipsis literalSet args)

    let parseSyntaxLiterals =
        function
        | SEmpty, _ -> Ok Set.empty
        | literals when isProperList literals ->
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

    let sSyntaxRules envs pos cont =
        function
        | (SSymbol ellipsis, _) :: literals :: rules ->
            match parseSyntaxLiterals literals with
            | Ok literalSet ->
                match rules |> mapResult parseSyntaxRule with
                | Ok parsedRules ->
                    let transformer envs' pos' cont' args =
                        parsedRules |> trySyntaxRules envs envs' pos' cont' ellipsis literalSet args

                    Ok(SSyntax transformer, pos) |> cont
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | literals :: rules ->
            match parseSyntaxLiterals literals with
            | Ok literalSet ->
                match rules |> mapResult parseSyntaxRule with
                | Ok parsedRules ->
                    let transformer envs' pos' cont' args =
                        parsedRules |> trySyntaxRules envs envs' pos' cont' "..." literalSet args

                    Ok(SSyntax transformer, pos) |> cont
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid syntax-rules parameter." |> cont

    [<TailCall>]
    let rec evalLetSyntaxTransformers envs pos cont body acc =
        function
        | [] -> body |> Eval.eachEval (Context.extendEnvs envs acc) cont (Ok(SEmpty, pos))
        | (var, expr) :: rest ->
            expr
            |> Eval.eval envs (function
                | Ok transformer ->
                    rest
                    |> evalLetSyntaxTransformers envs pos cont body ((var, ref transformer) :: acc)
                | x -> x |> cont)

    let sLetSyntax envs pos cont =
        function
        | bindings :: body ->
            match bindings |> toList with
            | Ok blist ->
                match blist |> mapResult eachBinding with
                | Ok bindings' -> bindings' |> evalLetSyntaxTransformers envs pos cont body []
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid let-syntax parameter." |> cont

    [<TailCall>]
    let rec evalLetRecSyntaxTransformers envs pos cont body =
        function
        | [], _
        | _, [] -> body |> Eval.eachEval envs cont (Ok(SEmpty, pos))
        | (_, expr) :: rest, r: SExpression ref :: restRefs ->
            expr
            |> Eval.eval envs (function
                | Ok transformer ->
                    r.Value <- transformer
                    (rest, restRefs) |> evalLetRecSyntaxTransformers envs pos cont body
                | x -> x |> cont)

    let sLetRecSyntax envs pos cont =
        function
        | bindings :: body ->
            match bindings |> toList with
            | Ok blist ->
                match blist |> mapResult eachBinding with
                | Ok bindings' ->
                    let vars = bindings' |> List.map (fun (v, _) -> v, ref (SEmpty, pos))
                    let envs' = vars |> Context.extendEnvs envs

                    (bindings', vars |> List.map snd)
                    |> evalLetRecSyntaxTransformers envs' pos cont body
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid letrec-syntax parameter." |> cont

    let sDefineSyntax envs pos cont =
        function
        | [ SSymbol keyword, _ as sym; transformer ] ->
            transformer
            |> Eval.eval
                envs
                (Result.map (fun x ->
                    Context.defineEnvVar envs keyword x
                    sym)
                 >> cont)
        | x -> x |> invalidParameter pos "'%s' invalid define-syntax parameter." |> cont

    let sSyntaxError envs pos cont =
        function
        | (SString message, _) :: irritants -> Error(SchemeRaise((SError(message, irritants), pos), pos)) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid syntax-error parameter." |> cont
