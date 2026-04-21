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
        | SPair p -> p.cdr |> decodePair (p.car :: acc)
        | x -> List.rev acc, x

    [<TailCall>]
    let rec loopPatternVars literals ellipsis acc =
        function
        | [] -> acc
        | x :: xs ->
            match x with
            | SSymbol "_" -> loopPatternVars literals ellipsis acc xs
            | SSymbol s when s = ellipsis -> loopPatternVars literals ellipsis acc xs
            | SSymbol s when Set.contains s literals -> loopPatternVars literals ellipsis acc xs
            | SSymbol s -> loopPatternVars literals ellipsis (s :: acc) xs
            | SPair _ as p ->
                let pats, tail = p |> decodePair []
                loopPatternVars literals ellipsis acc (pats @ tail :: xs)
            | SVector pats -> loopPatternVars literals ellipsis acc (Array.toList pats @ xs)
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
    let rec matchOne defEnvs useEnvs literals ellipsis inp cont =
        function
        | SSymbol "_" -> Map.empty |> Some |> cont
        | SSymbol s when s = ellipsis -> None |> cont
        | SSymbol s when Set.contains s literals ->
            if freeIdentifierEquals defEnvs (SSymbol s) useEnvs inp then
                Map.empty |> Some |> cont
            else
                None |> cont
        | SSymbol s -> Map.ofList [ s, SingleB inp ] |> Some |> cont
        | SEmpty ->
            match inp with
            | SEmpty -> Map.empty |> Some |> cont
            | _ -> None |> cont
        | SBool v ->
            match inp with
            | SBool v' when v = v' -> Map.empty |> Some |> cont
            | _ -> None |> cont
        | SRational(n1, d1) ->
            match inp with
            | SRational(n2, d2) when n1 = n2 && d1 = d2 -> Map.empty |> Some |> cont
            | _ -> None |> cont
        | SReal v ->
            match inp with
            | SReal v' when v = v' -> Map.empty |> Some |> cont
            | _ -> None |> cont
        | SString v ->
            match inp with
            | SString v' when v = v' -> Map.empty |> Some |> cont
            | _ -> None |> cont
        | SChar v ->
            match inp with
            | SChar v' when v = v' -> Map.empty |> Some |> cont
            | _ -> None |> cont
        | SPair { car = SSymbol ell
                  cdr = SPair { car = SSymbol s; cdr = SEmpty } } when ell = ellipsis && s = ellipsis ->
            match inp with
            | SSymbol s' when s' = ellipsis -> Map.empty |> Some |> cont
            | _ -> None |> cont
        | SPair _ as p ->
            let patList, patTail = p |> decodePair []
            let isProper = patTail = SEmpty

            match inp with
            | SPair _ as i when isProper && isProperList i ->
                matchPatternList defEnvs useEnvs literals ellipsis (i |> toList) cont patList
            | SPair _ as i when not isProper -> (p, i) |> loopMatchOnePair defEnvs useEnvs literals ellipsis cont
            | _ -> None |> cont
        | SVector patArray ->
            match inp with
            | SVector inpArray when patArray.Length = inpArray.Length ->
                Array.toList patArray
                |> matchPatternList defEnvs useEnvs literals ellipsis (Array.toList inpArray) cont
            | _ -> None |> cont
        | _ -> None |> cont

    and [<TailCall>] loopMatchOnePair defEnvs useEnvs literals ellipsis cont =
        function
        | SPair pp, SPair ii ->
            pp.car
            |> matchOne defEnvs useEnvs literals ellipsis ii.car (function
                | Some b1 ->
                    (pp.cdr, ii.cdr)
                    |> loopMatchOnePair defEnvs useEnvs literals ellipsis (function
                        | Some b2 -> mergeBindings b1 b2 |> Some |> cont
                        | None -> None |> cont)
                | None -> None |> cont)
        | pat, inp -> matchOne defEnvs useEnvs literals ellipsis inp cont pat

    and [<TailCall>] matchPatternList defEnvs useEnvs literals ellipsis inps cont pats =
        let dotIdx =
            pats
            |> List.tryFindIndex (function
                | SSymbol s when s = ellipsis -> true
                | _ -> false)

        match dotIdx with
        | Some i when i > 0 ->
            let prefix = pats |> List.take (i - 1)
            let ellipsisPat = pats.[i - 1]
            let suffix = pats |> List.skip (i + 1)
            let prefixCount = prefix.Length
            let suffixCount = suffix.Length
            let inpCount = inps.Length

            if inpCount < prefixCount + suffixCount then
                None |> cont
            else
                let prefixInps = inps |> List.take prefixCount
                let suffixInps = inps |> List.skip (inpCount - suffixCount)

                let ellipsisInps =
                    inps
                    |> List.skip prefixCount
                    |> List.take (inpCount - prefixCount - suffixCount)

                prefix
                |> matchPatternList defEnvs useEnvs literals ellipsis prefixInps (function
                    | Some b1 ->
                        suffix
                        |> matchPatternList defEnvs useEnvs literals ellipsis suffixInps (function
                            | Some b2 ->
                                let vars = collectPatternVars literals ellipsis ellipsisPat

                                ellipsisInps
                                |> matchEllipsis
                                    defEnvs
                                    useEnvs
                                    literals
                                    ellipsis
                                    (function
                                    | Some b3 -> mergeBindings (mergeBindings b1 b2) b3 |> Some |> cont
                                    | None -> None |> cont)
                                    ellipsisPat
                                    vars
                                    []
                            | None -> None |> cont)
                    | None -> None |> cont)
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
                                | Some b2' -> mergeBindings b1 b2' |> Some |> cont
                                | None -> None |> cont)
                        | None -> None |> cont)
                | [] -> None |> cont
            | [] ->
                if List.isEmpty inps then
                    Map.empty |> Some |> cont
                else
                    None |> cont

    and [<TailCall>] matchEllipsis defEnvs useEnvs literals ellipsis cont pat vars results =
        function
        | [] ->
            let allBindings = results |> List.rev |> List.map Option.get

            vars
            |> List.fold
                (fun acc var ->
                    let values =
                        allBindings
                        |> List.map (fun b ->
                            match Map.tryFind var b with
                            | Some b' -> b'
                            | None -> SingleB SEmpty)

                    Map.add var (EllipsisB values) acc)
                Map.empty
            |> Some
            |> cont
        | inp :: restInps ->
            pat
            |> matchOne defEnvs useEnvs literals ellipsis inp (function
                | Some b ->
                    restInps
                    |> matchEllipsis defEnvs useEnvs literals ellipsis cont pat vars (Some b :: results)
                | None -> None |> cont)

    [<TailCall>]
    let rec loopTemplateVars ellipsis acc =
        function
        | [] -> acc
        | x :: xs ->
            match x with
            | SSymbol s -> loopTemplateVars ellipsis (s :: acc) xs
            | SPair _ as p ->
                let elems, tail = p |> decodePair []
                loopTemplateVars ellipsis acc (elems @ tail :: xs)
            | SQuote x'
            | SQuasiquote x'
            | SUnquote x'
            | SUnquoteSplicing x' -> loopTemplateVars ellipsis acc (x' :: xs)
            | SVector elements -> loopTemplateVars ellipsis acc (Array.toList elements @ xs)
            | _ -> loopTemplateVars ellipsis acc xs

    let collectTemplateVars ellipsis x =
        [ x ] |> loopTemplateVars ellipsis [] |> List.distinct |> List.rev

    [<TailCall>]
    let rec renameTemplate toRename cont =
        function
        | SSymbol s ->
            match Map.tryFind s toRename with
            | Some s' -> SSymbol s' |> cont
            | None -> SSymbol s |> cont
        | SPair _ as p ->
            let carElements, cdr = p |> decodePair []
            let isProper = cdr = SEmpty

            carElements
            |> renameTemplateList toRename (fun resCar ->
                if isProper then
                    toSPair resCar |> cont
                else
                    cdr
                    |> renameTemplate toRename (fun resCdr ->
                        List.foldBack (fun x acc -> SPair { car = x; cdr = acc }) resCar resCdr |> cont))
        | SVector elements ->
            elements
            |> Array.toList
            |> renameTemplateList toRename (List.toArray >> SVector >> cont)
        | SQuote x -> x |> renameTemplate toRename (SQuote >> cont)
        | SQuasiquote x -> x |> renameTemplate toRename (SQuasiquote >> cont)
        | SUnquote x -> x |> renameTemplate toRename (SUnquote >> cont)
        | SUnquoteSplicing x -> x |> renameTemplate toRename (SUnquoteSplicing >> cont)
        | x -> x |> cont

    and [<TailCall>] renameTemplateList toRename cont =
        function
        | [] -> [] |> cont
        | x :: xs ->
            x
            |> renameTemplate toRename (fun resX ->
                xs |> renameTemplateList toRename (fun resXs -> resX :: resXs |> cont))

    [<TailCall>]
    let rec expandTemplate ellipsis isRaw cont bindings =
        function
        | SSymbol s ->
            match Map.tryFind s bindings with
            | Some(SingleB v) -> v |> cont
            | _ -> SSymbol s |> cont
        | SPair { car = SSymbol ell
                  cdr = SPair { car = tmpl; cdr = SEmpty } } when not isRaw && ell = ellipsis ->
            tmpl |> expandTemplate ellipsis true cont bindings
        | SPair _ as p ->
            let elems, tail = p |> decodePair []
            let isProper = tail = SEmpty

            elems
            |> expandTemplateList
                ellipsis
                isRaw
                (fun res ->
                    if isProper then
                        toSPair res |> cont
                    else
                        tail
                        |> expandTemplate
                            ellipsis
                            isRaw
                            (fun resTail ->
                                List.foldBack (fun x acc -> SPair { car = x; cdr = acc }) res resTail |> cont)
                            bindings)
                bindings
        | SQuote x -> x |> expandTemplate ellipsis isRaw (SQuote >> cont) bindings
        | SQuasiquote x -> x |> expandTemplate ellipsis isRaw (SQuasiquote >> cont) bindings
        | SUnquote x -> x |> expandTemplate ellipsis isRaw (SUnquote >> cont) bindings
        | SUnquoteSplicing x -> x |> expandTemplate ellipsis isRaw (SUnquoteSplicing >> cont) bindings
        | SVector elements ->
            elements
            |> Array.toList
            |> expandTemplateList ellipsis isRaw (List.toArray >> SVector >> cont) bindings
        | x -> x |> cont

    and [<TailCall>] expandTemplateList ellipsis isRaw cont bindings =
        function
        | [] -> [] |> cont
        | tmpl :: SSymbol s :: rest when not isRaw && s = ellipsis ->
            let ellipsisVars =
                collectTemplateVars ellipsis tmpl
                |> List.choose (fun v ->
                    match Map.tryFind v bindings with
                    | Some(EllipsisB values) -> Some(v, values)
                    | _ -> None)

            match ellipsisVars with
            | [] -> rest |> expandTemplateList ellipsis isRaw cont bindings
            | (_, firstValues) :: _ ->
                let count = firstValues.Length

                expandEllipsis
                    ellipsis
                    (fun expanded ->
                        rest
                        |> expandTemplateList
                            ellipsis
                            isRaw
                            (fun expandedRest -> expanded @ expandedRest |> cont)
                            bindings)
                    bindings
                    tmpl
                    ellipsisVars
                    count
                    0
                    []
        | tmpl :: rest ->
            tmpl
            |> expandTemplate
                ellipsis
                isRaw
                (fun expandedTmpl ->
                    rest
                    |> expandTemplateList
                        ellipsis
                        isRaw
                        (fun expandedRest -> expandedTmpl :: expandedRest |> cont)
                        bindings)
                bindings

    and [<TailCall>] expandEllipsis ellipsis cont bindings tmpl ellipsisVars count i acc =
        if i >= count then
            List.rev acc |> cont
        else
            let localBindings =
                ellipsisVars
                |> List.fold (fun acc (v, values) -> Map.add v values.[i] acc) bindings

            tmpl
            |> expandTemplate
                ellipsis
                false
                (fun res -> expandEllipsis ellipsis cont bindings tmpl ellipsisVars count (i + 1) (res :: acc))
                localBindings

    [<TailCall>]
    let rec trySyntaxRules defEnvs useEnvs cont ellipsis literalSet args =
        function
        | [] -> failwith "no matching syntax-rules pattern"
        | (patBody, template) :: rest ->
            patBody
            |> matchPatternList defEnvs useEnvs literalSet ellipsis args (function
                | Some bindings ->
                    let patternVars =
                        collectPatternVars literalSet ellipsis (toSPair patBody) |> Set.ofList

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
                        |> expandTemplate ellipsis false (Eval.eval extendedEnvs cont) bindings)
                | None -> rest |> trySyntaxRules defEnvs useEnvs cont ellipsis literalSet args)

    let sSyntaxRules envs cont =
        let parseLiterals =
            function
            | SEmpty -> Set.empty
            | x when isProperList x ->
                x
                |> toList
                |> List.choose (function
                    | SSymbol s -> Some s
                    | _ -> None)
                |> Set.ofList
            | x -> x |> invalid "'%s' invalid syntax-rules literals."

        let parseRule =
            function
            | SPair { car = SPair { car = _; cdr = patBody }
                      cdr = SPair { car = template; cdr = SEmpty } } -> patBody |> toList, template
            | x -> x |> invalid "'%s' invalid syntax-rules clause."

        function
        | SSymbol ell :: literals :: rules ->
            let literalSet = parseLiterals literals
            let parsedRules = rules |> List.map parseRule

            let transformer envs' cont' args =
                parsedRules |> trySyntaxRules envs envs' cont' ell literalSet args

            SSyntax transformer |> cont
        | literals :: rules ->
            let literalSet = parseLiterals literals
            let parsedRules = rules |> List.map parseRule

            let transformer envs' cont' args =
                parsedRules |> trySyntaxRules envs envs' cont' "..." literalSet args

            SSyntax transformer |> cont
        | x -> x |> invalidParameter "'%s' invalid syntax-rules parameter."

    let sSyntaxError envs cont =
        function
        | SString msg :: irritants -> raise (SchemeRaise(SError(msg, irritants)))
        | x -> x |> invalidParameter "'%s' invalid syntax-error parameter."

    [<TailCall>]
    let rec evalLetSyntaxTransformers envs cont body acc =
        function
        | [] -> body |> Eval.eachEval (acc |> List.rev |> Context.extendEnvs envs) cont SEmpty
        | (var, expr) :: rest ->
            expr
            |> Eval.eval envs (fun transformer ->
                rest |> evalLetSyntaxTransformers envs cont body ((var, ref transformer) :: acc))

    let sLetSyntax envs cont =
        function
        | bindings :: body ->
            bindings
            |> toList
            |> List.map eachBinding
            |> evalLetSyntaxTransformers envs cont body []
        | x -> x |> invalidParameter "'%s' invalid let-syntax parameter."

    [<TailCall>]
    let rec evalLetRecSyntaxTransformers envs cont body =
        function
        | [], _
        | _, [] -> body |> Eval.eachEval envs cont SEmpty
        | (_, expr) :: rest, r: SExpression ref :: restRefs ->
            expr
            |> Eval.eval envs (fun transformer ->
                r.Value <- transformer
                (rest, restRefs) |> evalLetRecSyntaxTransformers envs cont body)

    let sLetRecSyntax envs cont =
        function
        | bindings :: body ->
            let bindings' = bindings |> toList |> List.map eachBinding
            let vars = bindings' |> List.map (fun (v, _) -> v, ref SEmpty)
            let envs' = vars |> Context.extendEnvs envs

            (bindings', vars |> List.map snd)
            |> evalLetRecSyntaxTransformers envs' cont body
        | x -> x |> invalidParameter "'%s' invalid letrec-syntax parameter."

    let sDefineSyntax envs cont =
        function
        | [ SSymbol var; expr ] ->
            expr
            |> Eval.eval envs (fun x ->
                Context.defineEnvVar envs var x
                var |> SSymbol |> cont)
        | x -> x |> invalidParameter "'%s' invalid define-syntax parameter."
