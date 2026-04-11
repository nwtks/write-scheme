namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Macro =
    type SBinding =
        | SingleB of SExpression
        | EllipsisB of SExpression list

    let mergeBindings (b1: Map<string, SBinding>) (b2: Map<string, SBinding>) =
        Map.fold (fun acc k v -> Map.add k v acc) b1 b2

    [<TailCall>]
    let rec loopPatternVars (literals: Set<string>) acc =
        function
        | [] -> acc
        | x :: xs ->
            match x with
            | SSymbol "_" -> loopPatternVars literals acc xs
            | SSymbol "..." -> loopPatternVars literals acc xs
            | SSymbol s when Set.contains s literals -> loopPatternVars literals acc xs
            | SSymbol s -> loopPatternVars literals (s :: acc) xs
            | SList pats -> loopPatternVars literals acc (pats @ xs)
            | _ -> loopPatternVars literals acc xs

    let collectPatternVars literals x =
        [ x ] |> loopPatternVars literals [] |> List.distinct |> List.rev

    [<TailCall>]
    let rec matchOne (literals: Set<string>) (pat: SExpression) (inp: SExpression) : Map<string, SBinding> option =
        match pat with
        | SSymbol "_" -> Some Map.empty
        | SSymbol "..." -> None
        | SSymbol s when Set.contains s literals ->
            match inp with
            | SSymbol s' when s = s' -> Some Map.empty
            | _ -> None
        | SSymbol s -> Some(Map.ofList [ s, SingleB inp ])
        | SEmpty ->
            match inp with
            | SEmpty -> Some Map.empty
            | _ -> None
        | SBool v ->
            match inp with
            | SBool v' when v = v' -> Some Map.empty
            | _ -> None
        | SRational(n1, d1) ->
            match inp with
            | SRational(n2, d2) when n1 = n2 && d1 = d2 -> Some Map.empty
            | _ -> None
        | SReal v ->
            match inp with
            | SReal v' when v = v' -> Some Map.empty
            | _ -> None
        | SString v ->
            match inp with
            | SString v' when v = v' -> Some Map.empty
            | _ -> None
        | SChar v ->
            match inp with
            | SChar v' when v = v' -> Some Map.empty
            | _ -> None
        | SList patList ->
            match inp with
            | SList inpList -> matchPatternList literals patList inpList
            | _ -> None
        | _ -> None

    and matchPatternList
        (literals: Set<string>)
        (pats: SExpression list)
        (inps: SExpression list)
        : Map<string, SBinding> option =
        match pats with
        | [ patE; SSymbol "..." ] -> matchEllipsis literals patE inps
        | pat :: rest ->
            match inps with
            | inp :: restInps ->
                matchOne literals pat inp
                |> Option.bind (fun b1 -> matchPatternList literals rest restInps |> Option.map (mergeBindings b1))
            | [] -> None
        | [] -> if List.isEmpty inps then Some Map.empty else None

    and matchEllipsis
        (literals: Set<string>)
        (patE: SExpression)
        (inps: SExpression list)
        : Map<string, SBinding> option =
        let vars = collectPatternVars literals patE
        let results = inps |> List.map (matchOne literals patE)

        if results |> List.forall Option.isSome then
            let allBindings = results |> List.map Option.get

            let merged =
                vars
                |> List.fold
                    (fun acc var ->
                        let values =
                            allBindings
                            |> List.map (fun b ->
                                match Map.tryFind var b with
                                | Some(SingleB v) -> v
                                | _ -> SEmpty)

                        Map.add var (EllipsisB values) acc)
                    Map.empty

            Some merged
        else
            None

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
            | _ -> loopTemplateVars acc xs

    let collectTemplateVars x =
        [ x ] |> loopTemplateVars [] |> List.distinct |> List.rev

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
                |> List.fold (fun acc (v, values) -> Map.add v (SingleB values.[i]) acc) bindings

            expandTemplate
                (fun res -> expandEllipsis cont bindings tmpl ellipsisVars count (i + 1) (res :: acc))
                localBindings
                tmpl

    [<TailCall>]
    let rec trySyntaxRules envs cont literalSet args =
        function
        | [] -> failwith "no matching syntax-rules pattern"
        | (patBody, template) :: rest ->
            match matchPatternList literalSet patBody args with
            | Some bindings -> expandTemplate (Eval.eval envs cont) bindings template
            | None -> trySyntaxRules envs cont literalSet args rest

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
                trySyntaxRules envs' cont' literalSet args parsedRules

            SSyntax transformer |> cont
        | x -> x |> invalidParameter "'%s' invalid syntax-rules parameter."

    let sDefineSyntax (envs: SEnv list) cont =
        function
        | [ SSymbol var; expr ] ->
            envs.Head.TryAdd(var, ref SEmpty) |> ignore

            expr
            |> Eval.eval envs (fun x ->
                envs.Head.[var].Value <- x
                var |> SSymbol |> cont)
        | x -> x |> invalidParameter "'%s' invalid define-syntax parameter."
