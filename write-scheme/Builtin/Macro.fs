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
    let rec loopPatternVars literals acc =
        function
        | [] -> acc
        | x :: xs ->
            match x with
            | SSymbol "_" -> loopPatternVars literals acc xs
            | SSymbol "..." -> loopPatternVars literals acc xs
            | SSymbol s when Set.contains s literals -> loopPatternVars literals acc xs
            | SSymbol s -> loopPatternVars literals (s :: acc) xs
            | SList pats -> loopPatternVars literals acc (pats @ xs)
            | SPair(pats, p) -> loopPatternVars literals acc (pats @ [ p ] @ xs)
            | SVector pats -> loopPatternVars literals acc (Array.toList pats @ xs)
            | _ -> loopPatternVars literals acc xs

    let collectPatternVars literals x =
        [ x ] |> loopPatternVars literals [] |> List.distinct |> List.rev

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
    let rec matchOne defEnvs useEnvs literals inp cont =
        function
        | SSymbol "_" -> cont (Some Map.empty)
        | SSymbol "..." -> cont None
        | SSymbol s when Set.contains s literals ->
            if freeIdentifierEquals defEnvs (SSymbol s) useEnvs inp then
                cont (Some Map.empty)
            else
                cont None
        | SSymbol s -> cont (Some(Map.ofList [ s, SingleB inp ]))
        | SEmpty ->
            match inp with
            | SEmpty -> cont (Some Map.empty)
            | _ -> cont None
        | SBool v ->
            match inp with
            | SBool v' when v = v' -> cont (Some Map.empty)
            | _ -> cont None
        | SRational(n1, d1) ->
            match inp with
            | SRational(n2, d2) when n1 = n2 && d1 = d2 -> cont (Some Map.empty)
            | _ -> cont None
        | SReal v ->
            match inp with
            | SReal v' when v = v' -> cont (Some Map.empty)
            | _ -> cont None
        | SString v ->
            match inp with
            | SString v' when v = v' -> cont (Some Map.empty)
            | _ -> cont None
        | SChar v ->
            match inp with
            | SChar v' when v = v' -> cont (Some Map.empty)
            | _ -> cont None
        | SList patList ->
            match inp with
            | SList inpList -> patList |> matchPatternList defEnvs useEnvs literals inpList cont
            | _ -> cont None
        | SPair(patList, patTail) ->
            match inp with
            | SPair(inpList, inpTail) ->
                patList
                |> matchPatternList defEnvs useEnvs literals inpList (function
                    | Some b1 ->
                        patTail
                        |> matchOne defEnvs useEnvs literals inpTail (fun b2 ->
                            Option.map (mergeBindings b1) b2 |> cont)
                    | None -> cont None)
            | SList inpList when inpList.Length >= patList.Length ->
                patList
                |> matchPatternList defEnvs useEnvs literals (List.take patList.Length inpList) (function
                    | Some b1 ->
                        patTail
                        |> matchOne defEnvs useEnvs literals (toSList (List.skip patList.Length inpList)) (fun b2 ->
                            Option.map (mergeBindings b1) b2 |> cont)
                    | None -> cont None)
            | _ -> cont None
        | SVector patArray ->
            match inp with
            | SVector inpArray when patArray.Length = inpArray.Length ->
                Array.toList patArray
                |> matchPatternList defEnvs useEnvs literals (Array.toList inpArray) cont
            | _ -> cont None
        | _ -> cont None

    and matchPatternList defEnvs useEnvs literals inps cont =
        function
        | [ patE; SSymbol "..." ] ->
            inps
            |> matchEllipsis defEnvs useEnvs literals cont patE (collectPatternVars literals patE) []
        | pat :: rest ->
            match inps with
            | inp :: restInps ->
                pat
                |> matchOne defEnvs useEnvs literals inp (function
                    | Some b1 ->
                        rest
                        |> matchPatternList defEnvs useEnvs literals restInps (fun b2 ->
                            Option.map (mergeBindings b1) b2 |> cont)
                    | None -> cont None)
            | [] -> cont None
        | [] ->
            if List.isEmpty inps then
                cont (Some Map.empty)
            else
                cont None

    and matchEllipsis defEnvs useEnvs literals cont pat vars results =
        function
        | [] ->
            let allBindings = results |> List.rev |> List.map Option.get

            let merged =
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

            cont (Some merged)
        | inp :: restInps ->
            pat
            |> matchOne defEnvs useEnvs literals inp (function
                | Some b ->
                    restInps
                    |> matchEllipsis defEnvs useEnvs literals cont pat vars (Some b :: results)
                | None -> cont None)

    [<TailCall>]
    let rec loopTemplateVars acc =
        function
        | [] -> acc
        | x :: xs ->
            match x with
            | SSymbol s -> loopTemplateVars (s :: acc) xs
            | SList elems -> loopTemplateVars acc (elems @ xs)
            | SQuote x
            | SQuasiquote x
            | SUnquote x
            | SUnquoteSplicing x -> loopTemplateVars acc (x :: xs)
            | SPair(elems, x) -> loopTemplateVars acc (elems @ x :: xs)
            | SVector elements -> loopTemplateVars acc (Array.toList elements @ xs)
            | _ -> loopTemplateVars acc xs

    let collectTemplateVars x =
        [ x ] |> loopTemplateVars [] |> List.distinct |> List.rev

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
        | SVector elements ->
            renameTemplateList toRename (Array.toList elements) (fun res -> SVector(List.toArray res) |> cont)
        | SQuote x -> renameTemplate toRename x (SQuote >> cont)
        | SQuasiquote x -> renameTemplate toRename x (SQuasiquote >> cont)
        | SUnquote x -> renameTemplate toRename x (SUnquote >> cont)
        | SUnquoteSplicing x -> renameTemplate toRename x (SUnquoteSplicing >> cont)
        | x -> x |> cont

    and renameTemplateList toRename exprs cont =
        match exprs with
        | [] -> [] |> cont
        | x :: xs ->
            renameTemplate toRename x (fun resX -> renameTemplateList toRename xs (fun resXs -> resX :: resXs |> cont))

    [<TailCall>]
    let rec expandTemplate cont bindings =
        function
        | SSymbol s ->
            match Map.tryFind s bindings with
            | Some(SingleB v) -> v |> cont
            | _ -> SSymbol s |> cont
        | SList elems -> expandTemplateList (toSList >> cont) bindings elems
        | SQuote x -> expandTemplate (SQuote >> cont) bindings x
        | SQuasiquote x -> expandTemplate (SQuasiquote >> cont) bindings x
        | SUnquote x -> expandTemplate (SUnquote >> cont) bindings x
        | SUnquoteSplicing x -> expandTemplate (SUnquoteSplicing >> cont) bindings x
        | SPair(xs, x) ->
            expandTemplateList
                (fun (expandedXs: SExpression list) ->
                    expandTemplate (fun expandedX -> SPair(expandedXs, expandedX) |> cont) bindings x)
                bindings
                xs
        | SVector elements ->
            expandTemplateList
                (fun expandedElems -> SVector(List.toArray expandedElems) |> cont)
                bindings
                (Array.toList elements)
        | x -> x |> cont

    and expandTemplateList cont bindings =
        function
        | [] -> [] |> cont
        | tmpl :: SSymbol "..." :: rest ->
            let vars = collectTemplateVars tmpl

            let ellipsisVars =
                vars
                |> List.choose (fun v ->
                    match Map.tryFind v bindings with
                    | Some(EllipsisB values) -> Some(v, values)
                    | _ -> None)

            match ellipsisVars with
            | [] -> expandTemplateList cont bindings rest
            | (_, firstValues) :: _ ->
                let count = firstValues.Length

                expandEllipsis
                    (fun expanded ->
                        expandTemplateList (fun expandedRest -> expanded @ expandedRest |> cont) bindings rest)
                    bindings
                    tmpl
                    ellipsisVars
                    count
                    0
                    []
        | tmpl :: rest ->
            expandTemplate
                (fun expandedTmpl ->
                    expandTemplateList (fun expandedRest -> expandedTmpl :: expandedRest |> cont) bindings rest)
                bindings
                tmpl

    and expandEllipsis cont bindings tmpl ellipsisVars count i acc =
        if i >= count then
            List.rev acc |> cont
        else
            let localBindings =
                ellipsisVars
                |> List.fold (fun acc (v, values) -> Map.add v values.[i] acc) bindings

            expandTemplate
                (fun res -> expandEllipsis cont bindings tmpl ellipsisVars count (i + 1) (res :: acc))
                localBindings
                tmpl

    [<TailCall>]
    let rec trySyntaxRules defEnvs useEnvs cont literalSet args =
        function
        | [] -> failwith "no matching syntax-rules pattern"
        | (patBody, template) :: rest ->
            patBody
            |> matchPatternList defEnvs useEnvs literalSet args (function
                | Some bindings ->
                    let patternVars = collectPatternVars literalSet (SList patBody) |> Set.ofList

                    let templateVars =
                        collectTemplateVars template
                        |> List.filter (fun s ->
                            not (Set.contains s patternVars || Set.contains s literalSet || s = "..."))
                        |> List.distinct

                    let expansionId = Type.getNextExpansionId ()
                    let rename (s: string) = sprintf "%s#%d" s expansionId
                    let renameMap = templateVars |> List.map (fun s -> s, rename s) |> Map.ofList
                    let renamedTemplate = renameTemplate renameMap template id

                    let definitions =
                        templateVars
                        |> List.choose (fun s ->
                            match Eval.tryLookupEnvs defEnvs s with
                            | Some v -> Some(rename s, v)
                            | None -> None)

                    let extendedEnvs = Eval.extendEnvs useEnvs definitions
                    expandTemplate (Eval.eval extendedEnvs cont) bindings renamedTemplate
                | None -> trySyntaxRules defEnvs useEnvs cont literalSet args rest)

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
        | literals :: rules ->
            let literalSet = parseLiterals literals
            let parsedRules = rules |> List.map parseRule

            let transformer envs' cont' args =
                trySyntaxRules envs envs' cont' literalSet args parsedRules

            SSyntax transformer |> cont
        | x -> x |> invalidParameter "'%s' invalid syntax-rules parameter."

    let sSyntaxError envs cont =
        function
        | SString msg :: irritants -> raise (SchemeRaise(SError(msg, irritants)))
        | x -> x |> invalidParameter "'%s' invalid syntax-error parameter."

    [<TailCall>]
    let rec evalLetSyntaxTransformers envs cont body acc =
        function
        | [] -> body |> eachEval (acc |> List.rev |> Eval.extendEnvs envs) cont SEmpty
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
        | [], _ -> body |> eachEval envs cont SEmpty
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
            envs.Head.TryAdd(var, ref SEmpty) |> ignore

            expr
            |> Eval.eval envs (fun x ->
                envs.Head.[var].Value <- x
                var |> SSymbol |> cont)
        | x -> x |> invalidParameter "'%s' invalid define-syntax parameter."
