namespace WriteScheme.Builtins

open WriteScheme
open Type
open Eval
open Print

[<AutoOpen>]
module Macro =
    type SBinding =
        | SingleB of SExpression
        | EllipsisB of SExpression list

    let mergeBindings (b1: Map<string, SBinding>) (b2: Map<string, SBinding>) =
        Map.fold (fun acc k v -> Map.add k v acc) b1 b2

    let rec collectPatternVars (literals: Set<string>) =
        function
        | SSymbol "_" -> []
        | SSymbol "..." -> []
        | SSymbol s when Set.contains s literals -> []
        | SSymbol s -> [ s ]
        | SList pats -> pats |> List.collect (collectPatternVars literals)
        | _ -> []

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

    let rec collectTemplateVars =
        function
        | SSymbol s -> [ s ]
        | SList elems -> elems |> List.collect collectTemplateVars
        | SQuote x -> collectTemplateVars x
        | SQuasiquote x -> collectTemplateVars x
        | _ -> []

    let rec expandTemplate (bindings: Map<string, SBinding>) =
        function
        | SSymbol s ->
            match Map.tryFind s bindings with
            | Some(SingleB v) -> v
            | _ -> SSymbol s
        | SList elems -> expandTemplateList bindings elems |> newList
        | SQuote x -> SQuote(expandTemplate bindings x)
        | SQuasiquote x -> SQuasiquote(expandTemplate bindings x)
        | SUnquote x -> SUnquote(expandTemplate bindings x)
        | SUnquoteSplicing x -> SUnquoteSplicing(expandTemplate bindings x)
        | SPair(xs, x) -> SPair(xs |> List.map (expandTemplate bindings), expandTemplate bindings x)
        | x -> x

    and expandTemplateList (bindings: Map<string, SBinding>) =
        function
        | [] -> []
        | tmpl :: SSymbol "..." :: rest ->
            let vars = collectTemplateVars tmpl

            let ellipsisVars =
                vars
                |> List.choose (fun v ->
                    match Map.tryFind v bindings with
                    | Some(EllipsisB values) -> Some(v, values)
                    | _ -> None)

            match ellipsisVars with
            | [] -> expandTemplateList bindings rest
            | (_, firstValues) :: _ ->
                let count = firstValues.Length

                let expanded =
                    [ 0 .. count - 1 ]
                    |> List.map (fun i ->
                        let localBindings =
                            ellipsisVars
                            |> List.fold (fun acc (v, values) -> Map.add v (SingleB values.[i]) acc) bindings

                        expandTemplate localBindings tmpl)

                expanded @ expandTemplateList bindings rest
        | tmpl :: rest -> expandTemplate bindings tmpl :: expandTemplateList bindings rest

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
            | x -> print x |> sprintf "'%s' invalid syntax-rules literals." |> failwith

        let parseRule =
            function
            | SList [ SList(_ :: patBody); template ] -> (patBody, template)
            | x -> print x |> sprintf "'%s' invalid syntax-rules clause." |> failwith

        function
        | literals :: rules ->
            let literalSet = parseLiterals literals
            let parsedRules = rules |> List.map parseRule

            let transformer envs' cont' args =
                let rec tryRules =
                    function
                    | [] -> failwith "no matching syntax-rules pattern"
                    | (patBody, template) :: rest ->
                        match matchPatternList literalSet patBody args with
                        | Some bindings -> expandTemplate bindings template |> eval envs' cont'
                        | None -> tryRules rest

                tryRules parsedRules

            SSyntax transformer |> cont
        | x -> x |> invalidParameter "'%s' invalid syntax-rules parameter."

    let sDefineSyntax (envs: SEnv list) cont =
        function
        | [ SSymbol var; expr ] ->
            envs.Head.TryAdd(var, ref SEmpty) |> ignore

            expr
            |> eval envs (fun x ->
                envs.Head.[var].Value <- x
                var |> SSymbol |> cont)
        | x -> x |> invalidParameter "'%s' invalid define-syntax parameter."
