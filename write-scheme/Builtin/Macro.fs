namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Macro =
    type SBinding =
        | SingleB of SExpression
        | EllipsisB of SBinding list

    let mergeBindings b1 b2 =
        Map.fold (fun acc k v -> Map.add k v acc) b1 b2

    [<TailCall>]
    let rec decodePair acc =
        function
        | SPair p, _ -> p.cdr |> decodePair (p.car :: acc)
        | x -> List.rev acc, x

    [<TailCall>]
    let rec loopPatternVars literals ellipsis acc =
        function
        | [] -> acc
        | x :: xs ->
            match x with
            | SSymbol "_", _ -> loopPatternVars literals ellipsis acc xs
            | SSymbol s, _ when s = ellipsis -> loopPatternVars literals ellipsis acc xs
            | SSymbol s, _ when Set.contains s literals -> loopPatternVars literals ellipsis acc xs
            | SSymbol s, _ -> loopPatternVars literals ellipsis (s :: acc) xs
            | SPair _, _ as pair ->
                let elems, tail = pair |> decodePair []
                loopPatternVars literals ellipsis acc (elems @ tail :: xs)
            | SVector pats, _ -> loopPatternVars literals ellipsis acc (Array.toList pats @ xs)
            | _ -> loopPatternVars literals ellipsis acc xs

    let collectPatternVars literals ellipsis x =
        [ x ] |> loopPatternVars literals ellipsis [] |> List.distinct |> List.rev

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
        | SSymbol s as sym, _ when Set.contains s literals ->
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
            let elems, tail = pair |> decodePair []
            let isProper = fst tail = SEmpty

            match inp with
            | SPair _, _ as i when isProper && isProperList i ->
                matchPatternList defEnvs useEnvs literals ellipsis (i |> toList) next elems
            | SPair _, _ as i when not isProper -> (pair, i) |> loopMatchOnePair defEnvs useEnvs literals ellipsis next
            | _ -> None |> next
        | SVector patArray, _ ->
            match inp with
            | SVector inpArray, _ when patArray.Length = inpArray.Length ->
                Array.toList patArray
                |> matchPatternList defEnvs useEnvs literals ellipsis (Array.toList inpArray) next
            | _ -> None |> next
        | _ -> None |> next

    and [<TailCall>] loopMatchOnePair defEnvs useEnvs literals ellipsis next =
        function
        | (SPair pp, _), (SPair ii, _) ->
            pp.car
            |> matchOne defEnvs useEnvs literals ellipsis ii.car (function
                | Some b1 ->
                    (pp.cdr, ii.cdr)
                    |> loopMatchOnePair defEnvs useEnvs literals ellipsis (function
                        | Some b2 -> mergeBindings b1 b2 |> Some |> next
                        | None -> None |> next)
                | None -> None |> next)
        | pat, inp -> matchOne defEnvs useEnvs literals ellipsis inp next pat

    and [<TailCall>] matchPatternList defEnvs useEnvs literals ellipsis inps next pats =
        let dotIdx =
            pats
            |> List.tryFindIndex (function
                | SSymbol s, _ when s = ellipsis -> true
                | _ -> false)

        match dotIdx with
        | Some i when i > 0 ->
            let prefix = pats |> List.take (i - 1)
            let suffix = pats |> List.skip (i + 1)

            if inps.Length < prefix.Length + suffix.Length then
                None |> next
            else
                let ellipsisPat = pats.[i - 1]
                let prefixInps = inps |> List.take prefix.Length
                let suffixInps = inps |> List.skip (inps.Length - suffix.Length)

                let ellipsisInps =
                    inps
                    |> List.skip prefix.Length
                    |> List.take (inps.Length - prefix.Length - suffix.Length)

                prefix
                |> matchPatternList defEnvs useEnvs literals ellipsis prefixInps (function
                    | Some b1 ->
                        suffix
                        |> matchPatternList defEnvs useEnvs literals ellipsis suffixInps (function
                            | Some b2 ->
                                let vars = collectPatternVars literals ellipsis ellipsisPat

                                ellipsisInps
                                |> matchEllipsis defEnvs useEnvs literals ellipsis ellipsisPat vars [] (function
                                    | Some b3 -> mergeBindings (mergeBindings b1 b2) b3 |> Some |> next
                                    | None -> None |> next)
                            | None -> None |> next)
                    | None -> None |> next)
        | _ ->
            match pats with
            | pat :: rest ->
                match inps with
                | inp :: restInps ->
                    pat
                    |> matchOne defEnvs useEnvs literals ellipsis inp (function
                        | Some b1 ->
                            rest
                            |> matchPatternList defEnvs useEnvs literals ellipsis restInps (fun b2 ->
                                match b2 with
                                | Some b2' -> mergeBindings b1 b2' |> Some |> next
                                | None -> None |> next)
                        | None -> None |> next)
                | [] -> None |> next
            | [] ->
                if List.isEmpty inps then
                    Map.empty |> Some |> next
                else
                    None |> next

    and [<TailCall>] matchEllipsis defEnvs useEnvs literals ellipsis pat vars results next =
        function
        | [] ->
            let bindings = results |> List.rev |> List.map Option.get

            vars
            |> List.fold
                (fun acc var ->
                    let values =
                        bindings
                        |> List.map (fun b ->
                            match Map.tryFind var b with
                            | Some b' -> b'
                            | None -> SingleB(SEmpty, None))

                    Map.add var (EllipsisB values) acc)
                Map.empty
            |> Some
            |> next
        | inp :: restInps ->
            pat
            |> matchOne defEnvs useEnvs literals ellipsis inp (function
                | Some b ->
                    restInps
                    |> matchEllipsis defEnvs useEnvs literals ellipsis pat vars (Some b :: results) next
                | None -> None |> next)

    [<TailCall>]
    let rec loopTemplateVars ellipsis acc =
        function
        | [] -> acc
        | x :: xs ->
            match x with
            | SSymbol v, _ -> loopTemplateVars ellipsis (v :: acc) xs
            | SPair _, _ as pair ->
                let elems, tail = pair |> decodePair []
                loopTemplateVars ellipsis acc (elems @ tail :: xs)
            | SQuote v, _ -> loopTemplateVars ellipsis acc (v :: xs)
            | SQuasiquote v, _ -> loopTemplateVars ellipsis acc (v :: xs)
            | SUnquote v, _ -> loopTemplateVars ellipsis acc (v :: xs)
            | SUnquoteSplicing v, _ -> loopTemplateVars ellipsis acc (v :: xs)
            | SVector v, _ -> loopTemplateVars ellipsis acc ((v |> Array.toList) @ xs)
            | _ -> loopTemplateVars ellipsis acc xs

    let collectTemplateVars ellipsis x =
        [ x ] |> loopTemplateVars ellipsis [] |> List.distinct |> List.rev

    [<TailCall>]
    let rec renameTemplate toRename next =
        function
        | SSymbol x, pos as sym ->
            match Map.tryFind x toRename with
            | Some s -> (SSymbol s, pos) |> next
            | None -> sym |> next
        | SPair _, _ as pair ->
            let elems, tail = pair |> decodePair []
            let isProper = fst tail = SEmpty

            elems
            |> renameTemplateList toRename (fun resCar ->
                if isProper then
                    toSPair resCar |> next
                else
                    tail
                    |> renameTemplate toRename (fun resCdr ->
                        List.foldBack (fun x acc -> SPair { car = x; cdr = acc }, snd x) resCar resCdr
                        |> next))
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
            |> renameTemplate toRename (fun resX ->
                xs |> renameTemplateList toRename (fun resXs -> resX :: resXs |> next))

    [<TailCall>]
    let rec expandTemplate ellipsis isRaw bindings next =
        function
        | SSymbol x, _ as sym ->
            match Map.tryFind x bindings with
            | Some(SingleB v) -> v |> next
            | _ -> sym |> next
        | SPair { car = SSymbol ell, _
                  cdr = SPair { car = tmpl; cdr = SEmpty, _ }, _ },
          _ when not isRaw && ell = ellipsis -> tmpl |> expandTemplate ellipsis true bindings next
        | SPair _, _ as pair ->
            let elems, tail = pair |> decodePair []
            let isProper = fst tail = SEmpty

            elems
            |> expandTemplateList ellipsis isRaw bindings (fun res ->
                if isProper then
                    toSPair res |> next
                else
                    tail
                    |> expandTemplate ellipsis isRaw bindings (fun resTail ->
                        List.foldBack (fun x acc -> SPair { car = x; cdr = acc }, snd x) res resTail
                        |> next))
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
        | tmpl :: (SSymbol s, _) :: rest when not isRaw && s = ellipsis ->
            let ellipsisVars =
                collectTemplateVars ellipsis tmpl
                |> List.choose (fun v ->
                    match Map.tryFind v bindings with
                    | Some(EllipsisB values) -> Some(v, values)
                    | _ -> None)

            match ellipsisVars with
            | [] -> rest |> expandTemplateList ellipsis isRaw bindings next
            | (_, firstValues) :: _ ->
                let count = firstValues.Length

                expandEllipsis
                    ellipsis
                    bindings
                    tmpl
                    ellipsisVars
                    count
                    0
                    (fun expanded ->
                        rest
                        |> expandTemplateList ellipsis isRaw bindings (fun expandedRest ->
                            expanded @ expandedRest |> next))
                    []
        | tmpl :: rest ->
            tmpl
            |> expandTemplate ellipsis isRaw bindings (fun expandedTmpl ->
                rest
                |> expandTemplateList ellipsis isRaw bindings (fun expandedRest ->
                    expandedTmpl :: expandedRest |> next))

    and [<TailCall>] expandEllipsis ellipsis bindings tmpl ellipsisVars count i next acc =
        if i >= count then
            List.rev acc |> next
        else
            let localBindings =
                ellipsisVars
                |> List.fold (fun acc (v, values) -> Map.add v values.[i] acc) bindings

            tmpl
            |> expandTemplate ellipsis false localBindings (fun res ->
                res :: acc
                |> expandEllipsis ellipsis bindings tmpl ellipsisVars count (i + 1) next)

    [<TailCall>]
    let rec trySyntaxRules defEnvs useEnvs pos cont ellipsis literalSet args =
        function
        | [] -> failwithf "no matching syntax-rules pattern.%s" (pos |> formatPosition)
        | (patBody, template) :: rest ->
            patBody
            |> matchPatternList defEnvs useEnvs literalSet ellipsis args (function
                | Some bindings ->
                    let patternVars =
                        collectPatternVars literalSet ellipsis (patBody |> toSPair) |> Set.ofList

                    let templateVars =
                        collectTemplateVars ellipsis template
                        |> List.filter (fun s ->
                            not (Set.contains s patternVars || Set.contains s literalSet || s = ellipsis))
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
        | SEmpty, _ -> Set.empty
        | x when isProperList x ->
            x
            |> toList
            |> List.choose (function
                | SSymbol s, _ -> Some s
                | _ -> None)
            |> Set.ofList
        | x -> x |> invalid (snd x) "'%s' invalid syntax-rules literals."

    let parseSyntaxRule =
        function
        | SPair { car = SPair { car = _; cdr = patBody }, _
                  cdr = SPair { car = template; cdr = SEmpty, _ }, _ },
          _ -> patBody |> toList, template
        | x -> x |> invalid (snd x) "'%s' invalid syntax-rules clause."

    let sSyntaxRules envs pos cont =
        function
        | (SSymbol ell, _) :: literals :: rules ->
            let literalSet = parseSyntaxLiterals literals
            let parsedRules = rules |> List.map parseSyntaxRule

            let transformer envs' pos' cont' args =
                parsedRules |> trySyntaxRules envs envs' pos' cont' ell literalSet args

            (SSyntax transformer, pos) |> cont
        | literals :: rules ->
            let literalSet = parseSyntaxLiterals literals
            let parsedRules = rules |> List.map parseSyntaxRule

            let transformer envs' pos' cont' args =
                parsedRules |> trySyntaxRules envs envs' pos' cont' "..." literalSet args

            (SSyntax transformer, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid syntax-rules parameter."

    [<TailCall>]
    let rec evalLetSyntaxTransformers envs pos cont body acc =
        function
        | [] -> body |> Eval.eachEval envs cont (SEmpty, pos)
        | (var, expr) :: rest ->
            expr
            |> Eval.eval envs (fun transformer ->
                rest
                |> evalLetSyntaxTransformers envs pos cont body ((var, ref transformer) :: acc))

    let sLetSyntax envs pos cont =
        function
        | bindings :: body ->
            bindings
            |> toList
            |> List.map eachBinding
            |> evalLetSyntaxTransformers envs pos cont body []
        | x -> x |> invalidParameter pos "'%s' invalid let-syntax parameter."

    [<TailCall>]
    let rec evalLetRecSyntaxTransformers envs pos cont body =
        function
        | [], _
        | _, [] -> body |> Eval.eachEval envs cont (SEmpty, pos)
        | (_, expr) :: rest, r: SExpression ref :: restRefs ->
            expr
            |> Eval.eval envs (fun transformer ->
                r.Value <- transformer
                (rest, restRefs) |> evalLetRecSyntaxTransformers envs pos cont body)

    let sLetRecSyntax envs pos cont =
        function
        | bindings :: body ->
            let bindings' = bindings |> toList |> List.map eachBinding
            let vars = bindings' |> List.map (fun (v, _) -> v, ref (SEmpty, pos))
            let envs' = vars |> Context.extendEnvs envs

            (bindings', vars |> List.map snd)
            |> evalLetRecSyntaxTransformers envs' pos cont body
        | x -> x |> invalidParameter pos "'%s' invalid letrec-syntax parameter."

    let sDefineSyntax envs pos cont =
        function
        | [ SSymbol var, _ as sym; expr ] ->
            expr
            |> Eval.eval envs (fun x ->
                Context.defineEnvVar envs var x
                sym |> cont)
        | x -> x |> invalidParameter pos "'%s' invalid define-syntax parameter."

    let sSyntaxError envs pos cont =
        function
        | (SString msg, _) :: irritants -> SchemeRaise(SError(msg, irritants), pos) |> raise
        | x -> x |> invalidParameter pos "'%s' invalid syntax-error parameter."
