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
    let rec loopPatternVars literals ellipsis acc =
        function
        | [] -> acc
        | x :: xs ->
            match x with
            | SSymbol "_" -> loopPatternVars literals ellipsis acc xs
            | SSymbol s when s = ellipsis -> loopPatternVars literals ellipsis acc xs
            | SSymbol s when Set.contains s literals -> loopPatternVars literals ellipsis acc xs
            | SSymbol s -> loopPatternVars literals ellipsis (s :: acc) xs
            | SList pats -> loopPatternVars literals ellipsis acc (pats @ xs)
            | SPair(pats, p) -> loopPatternVars literals ellipsis acc (pats @ [ p ] @ xs)
            | SVector pats -> loopPatternVars literals ellipsis acc (Array.toList pats @ xs)
            | _ -> loopPatternVars literals ellipsis acc xs

    let collectPatternVars literals ellipsis x =
        [ x ] |> loopPatternVars literals ellipsis [] |> List.distinct |> List.rev

    let freeIdentifierEquals defEnvs id1 useEnvs id2 =
        match id1, id2 with
        | SSymbol s1, SSymbol s2 ->
            let ref1 = Eval.tryLookupEnvs defEnvs s1
            let ref2 = Eval.tryLookupEnvs useEnvs s2

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
        | SList [ SSymbol ell; SSymbol s ] when ell = ellipsis && s = ellipsis ->
            match inp with
            | SSymbol s' when s' = ellipsis -> Map.empty |> Some |> cont
            | _ -> None |> cont
        | SList patList ->
            match inp with
            | SList inpList -> patList |> matchPatternList defEnvs useEnvs literals ellipsis inpList cont
            | _ -> None |> cont
        | SPair(patList, patTail) ->
            match inp with
            | SPair(inpList, inpTail) ->
                patList
                |> matchPatternList defEnvs useEnvs literals ellipsis inpList (function
                    | Some b1 ->
                        patTail
                        |> matchOne defEnvs useEnvs literals ellipsis inpTail (fun b2 ->
                            Option.map (mergeBindings b1) b2 |> cont)
                    | None -> None |> cont)
            | SList inpList when inpList.Length >= patList.Length ->
                patList
                |> matchPatternList defEnvs useEnvs literals ellipsis (List.take patList.Length inpList) (function
                    | Some b1 ->
                        patTail
                        |> matchOne
                            defEnvs
                            useEnvs
                            literals
                            ellipsis
                            (inpList |> List.skip patList.Length |> toSList)
                            (fun b2 -> Option.map (mergeBindings b1) b2 |> cont)
                    | None -> None |> cont)
            | _ -> None |> cont
        | SVector patArray ->
            match inp with
            | SVector inpArray when patArray.Length = inpArray.Length ->
                Array.toList patArray
                |> matchPatternList defEnvs useEnvs literals ellipsis (Array.toList inpArray) cont
            | _ -> None |> cont
        | _ -> None |> cont

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
                                Option.map (mergeBindings b1) b2 |> cont)
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
            | SList elems -> loopTemplateVars ellipsis acc (elems @ xs)
            | SQuote x
            | SQuasiquote x
            | SUnquote x
            | SUnquoteSplicing x -> loopTemplateVars ellipsis acc (x :: xs)
            | SPair(elems, x) -> loopTemplateVars ellipsis acc (elems @ x :: xs)
            | SVector elements -> loopTemplateVars ellipsis acc (Array.toList elements @ xs)
            | _ -> loopTemplateVars ellipsis acc xs

    let collectTemplateVars ellipsis x =
        [ x ] |> loopTemplateVars ellipsis [] |> List.distinct |> List.rev

    [<TailCall>]
    let rec renameTemplate toRename expr cont =
        match expr with
        | SSymbol s ->
            match Map.tryFind s toRename with
            | Some s' -> SSymbol s' |> cont
            | None -> SSymbol s |> cont
        | SList elements -> renameTemplateList toRename elements (toSList >> cont)
        | SPair(carElements, cdr) ->
            renameTemplateList toRename carElements (fun resCar ->
                renameTemplate toRename cdr (fun resCdr -> SPair(resCar, resCdr) |> cont))
        | SVector elements -> renameTemplateList toRename (Array.toList elements) (List.toArray >> SVector >> cont)
        | SQuote x -> renameTemplate toRename x (SQuote >> cont)
        | SQuasiquote x -> renameTemplate toRename x (SQuasiquote >> cont)
        | SUnquote x -> renameTemplate toRename x (SUnquote >> cont)
        | SUnquoteSplicing x -> renameTemplate toRename x (SUnquoteSplicing >> cont)
        | x -> x |> cont

    and [<TailCall>] renameTemplateList toRename exprs cont =
        match exprs with
        | [] -> [] |> cont
        | x :: xs ->
            renameTemplate toRename x (fun resX -> renameTemplateList toRename xs (fun resXs -> resX :: resXs |> cont))

    [<TailCall>]
    let rec expandTemplate ellipsis isRaw cont bindings =
        function
        | SSymbol s ->
            match Map.tryFind s bindings with
            | Some(SingleB v) -> v |> cont
            | _ -> SSymbol s |> cont
        | SList [ SSymbol ell; tmpl ] when not isRaw && ell = ellipsis ->
            tmpl |> expandTemplate ellipsis true cont bindings
        | SList elems -> elems |> expandTemplateList ellipsis isRaw (toSList >> cont) bindings
        | SQuote x -> x |> expandTemplate ellipsis isRaw (SQuote >> cont) bindings
        | SQuasiquote x -> x |> expandTemplate ellipsis isRaw (SQuasiquote >> cont) bindings
        | SUnquote x -> x |> expandTemplate ellipsis isRaw (SUnquote >> cont) bindings
        | SUnquoteSplicing x -> x |> expandTemplate ellipsis isRaw (SUnquoteSplicing >> cont) bindings
        | SPair(xs, x) ->
            xs
            |> expandTemplateList
                ellipsis
                isRaw
                (fun expandedXs ->
                    x
                    |> expandTemplate ellipsis isRaw (fun expandedX -> SPair(expandedXs, expandedX) |> cont) bindings)
                bindings
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

    let private nextExpansionId = new System.Threading.ThreadLocal<int>(fun () -> 0)

    let getNextExpansionId () =
        let id = nextExpansionId.Value
        nextExpansionId.Value <- id + 1
        id

    [<TailCall>]
    let rec trySyntaxRules defEnvs useEnvs cont ellipsis literalSet args =
        function
        | [] -> failwith "no matching syntax-rules pattern"
        | (patBody, template) :: rest ->
            patBody
            |> matchPatternList defEnvs useEnvs literalSet ellipsis args (function
                | Some bindings ->
                    let patternVars =
                        collectPatternVars literalSet ellipsis (toSList patBody) |> Set.ofList

                    let templateVars =
                        collectTemplateVars ellipsis template
                        |> List.filter (fun s ->
                            not (Set.contains s patternVars || Set.contains s literalSet || s = ellipsis))
                        |> List.distinct

                    let expansionId = getNextExpansionId ()
                    let rename s = sprintf "%s#%d" s expansionId
                    let renameMap = templateVars |> List.map (fun s -> s, rename s) |> Map.ofList
                    let renamedTemplate = renameTemplate renameMap template id

                    let definitions =
                        templateVars
                        |> List.choose (fun s ->
                            match Eval.tryLookupEnvs defEnvs s with
                            | Some v -> Some(rename s, v)
                            | None -> None)

                    let extendedEnvs = Eval.extendEnvs useEnvs definitions

                    renamedTemplate
                    |> expandTemplate ellipsis false (Eval.eval extendedEnvs cont) bindings
                | None -> rest |> trySyntaxRules defEnvs useEnvs cont ellipsis literalSet args)

    let sSyntaxRules envs cont =
        let parseLiterals =
            function
            | SEmpty -> Set.empty
            | SList xs ->
                xs
                |> List.choose (function
                    | SSymbol s -> Some s
                    | _ -> None)
                |> Set.ofList
            | x -> Print.print x |> sprintf "'%s' invalid syntax-rules literals." |> failwith

        let parseRule =
            function
            | SList [ SList(_ :: patBody); template ] -> patBody, template
            | x -> Print.print x |> sprintf "'%s' invalid syntax-rules clause." |> failwith

        function
        | SSymbol ell :: literals :: rules ->
            let literalSet = parseLiterals literals
            let parsedRules = rules |> List.map parseRule

            let transformer envs' cont' args =
                trySyntaxRules envs envs' cont' ell literalSet args parsedRules

            SSyntax transformer |> cont
        | literals :: rules ->
            let literalSet = parseLiterals literals
            let parsedRules = rules |> List.map parseRule

            let transformer envs' cont' args =
                trySyntaxRules envs envs' cont' "..." literalSet args parsedRules

            SSyntax transformer |> cont
        | x -> x |> invalidParameter "'%s' invalid syntax-rules parameter."

    let sSyntaxError envs cont =
        function
        | SString msg :: irritants -> raise (SchemeRaise(SError(msg, irritants)))
        | x -> x |> invalidParameter "'%s' invalid syntax-error parameter."

    [<TailCall>]
    let rec evalLetSyntaxTransformers envs cont body acc =
        function
        | [] -> body |> Eval.eachEval (acc |> List.rev |> Eval.extendEnvs envs) cont SEmpty
        | (var, expr) :: rest ->
            expr
            |> Eval.eval envs (fun transformer ->
                rest |> evalLetSyntaxTransformers envs cont body ((var, ref transformer) :: acc))

    let sLetSyntax envs cont =
        function
        | SList bindings :: body -> bindings |> List.map eachBinding |> evalLetSyntaxTransformers envs cont body []
        | x -> x |> invalidParameter "'%s' invalid let-syntax parameter."

    [<TailCall>]
    let rec evalLetRecSyntaxTransformers envs cont body =
        function
        | [], _ -> body |> Eval.eachEval envs cont SEmpty
        | (_, expr) :: rest, r: SExpression ref :: restRefs ->
            expr
            |> Eval.eval envs (fun transformer ->
                r.Value <- transformer
                (rest, restRefs) |> evalLetRecSyntaxTransformers envs cont body)
        | _ -> failwith "invalid letrec-syntax state"

    let sLetRecSyntax envs cont =
        function
        | SList bindings :: body ->
            let bindings' = bindings |> List.map eachBinding
            let vars = bindings' |> List.map (fun (v, _) -> v, ref SEmpty)
            let envs' = vars |> Eval.extendEnvs envs

            (bindings', vars |> List.map snd)
            |> evalLetRecSyntaxTransformers envs' cont body
        | x -> x |> invalidParameter "'%s' invalid letrec-syntax parameter."

    let sDefineSyntax (envs: SEnv list) cont =
        function
        | [ SSymbol var; expr ] ->
            expr
            |> Eval.eval envs (fun x ->
                Eval.defineEnvVar envs var x
                var |> SSymbol |> cont)
        | x -> x |> invalidParameter "'%s' invalid define-syntax parameter."
