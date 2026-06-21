namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Quasiquote =
    type QqKeyword =
        | QqUnquote of SExpression
        | QqUnquoteSplicing of SExpression
        | QqQuasiquote of SExpression
        | QqQuote of SExpression

    let isQqKeyword (s: string) =
        s = "unquote" || s = "unquote-splicing" || s = "quasiquote" || s = "quote"

    let keywordToQq s t =
        match s with
        | "unquote" -> Some(QqUnquote t)
        | "unquote-splicing" -> Some(QqUnquoteSplicing t)
        | "quasiquote" -> Some(QqQuasiquote t)
        | _ -> Some(QqQuote t)

    let normalizeQqKeyword =
        function
        | SUnquote t, _ -> Some(QqUnquote t)
        | SUnquoteSplicing t, _ -> Some(QqUnquoteSplicing t)
        | SQuasiquote t, _ -> Some(QqQuasiquote t)
        | SQuote t, _ -> Some(QqQuote t)
        | SPair { car = SSymbol s, _
                  cdr = SPair { car = t; cdr = SEmpty, _ }, _ },
          _ when isQqKeyword s -> keywordToQq s t
        | _ -> None

    let consQq x b =
        match b with
        | SEmpty, _ -> [ x ] |> toSPair
        | SPair _, p -> SPair { car = x; cdr = b }, p
        | y -> SPair { car = x; cdr = y }, snd y

    let joinQq a b pos =
        match a with
        | SEmpty, _ -> Ok b
        | SPair _, _ ->
            match a |> toList with
            | Ok alist ->
                try
                    Ok(b |> List.foldBack (fun h acc -> SPair { car = h; cdr = acc }, snd h) alist)
                with _ ->
                    EvalError("unquote-splicing must return a list.", pos) |> Error
            | Error e -> Error e
        | x -> x |> invalid (snd x) "'%s' invalid unquote-splicing parameter."

    [<TailCall>]
    let rec loopReplaceQuasiquote acc =
        function
        | SPair template, _ -> template.cdr |> loopReplaceQuasiquote (template.car :: acc)
        | x -> acc |> List.rev, x

    [<TailCall>]
    let rec replaceQuasiquote context pos cont n next =
        function
        | SEmpty, _ -> Ok(SEmpty, pos) |> next
        | SPair _, _ as template ->
            let templates, templateTail = template |> loopReplaceQuasiquote []
            templates |> replaceQuasiquoteList context pos cont n next templateTail
        | SVector templates, _ ->
            templates
            |> Array.toList
            |> replaceQuasiquoteList
                context
                pos
                cont
                n
                (function
                | Ok(SEmpty, _) -> Ok([||] |> SVector, pos) |> next
                | Ok(SPair _, _ as y) ->
                    match y |> toList with
                    | Ok ylist -> Ok(ylist |> List.toArray |> SVector, pos) |> next
                    | Error e -> Error e |> next
                | x -> x |> next)
                (SEmpty, pos)
        | x -> x |> replaceQuasiquoteDatum context pos cont n next

    and [<TailCall>] chainExpand context pos cont n deltaN rest templateTail wrap next template =
        template
        |> replaceQuasiquote context pos cont (n + deltaN) (function
            | Ok a ->
                rest
                |> replaceQuasiquoteList
                    context
                    pos
                    cont
                    n
                    (Result.map (fun b -> consQq (wrap a) b) >> next)
                    templateTail
            | x -> x |> next)

    and [<TailCall>] replaceQuasiquoteList context pos cont n next templateTail templates =
        match templates with
        | [] -> templateTail |> replaceQuasiquoteDatum context pos cont n next
        | template :: rest ->
            match template |> normalizeQqKeyword with
            | Some(QqUnquote template) ->
                if n = 0 then
                    template
                    |> Eval.eval context (function
                        | Ok(SValues _, p) -> EvalError("Multiple values in single value context.", p) |> Error |> next
                        | Ok a ->
                            rest
                            |> replaceQuasiquoteList
                                context
                                pos
                                cont
                                n
                                (Result.map (fun b -> consQq a b) >> next)
                                templateTail
                        | x -> x |> next)
                else
                    template
                    |> chainExpand context pos cont n -1 rest templateTail (fun a -> (SUnquote a, pos)) next
            | Some(QqUnquoteSplicing template) ->
                if n = 0 then
                    template
                    |> Eval.eval context (function
                        | Ok a ->
                            rest
                            |> replaceQuasiquoteList
                                context
                                pos
                                cont
                                n
                                (Result.bind (fun b -> joinQq a b pos) >> next)
                                templateTail
                        | x -> x |> next)
                else
                    template
                    |> chainExpand context pos cont n -1 rest templateTail (fun a -> (SUnquoteSplicing a, pos)) next
            | Some(QqQuasiquote template) ->
                template
                |> chainExpand context pos cont n 1 rest templateTail (fun a -> (SQuasiquote a, pos)) next
            | Some(QqQuote template) ->
                template
                |> chainExpand context pos cont n 0 rest templateTail (fun a -> (SQuote a, pos)) next
            | None -> template |> chainExpand context pos cont n 0 rest templateTail id next

    and [<TailCall>] replaceQuasiquoteDatum context pos cont n next =
        function
        | arg ->
            match arg |> normalizeQqKeyword with
            | Some(QqUnquote template) ->
                if n = 0 then
                    template
                    |> Eval.eval context (function
                        | Ok(SValues _, p) -> EvalError("Multiple values in single value context.", p) |> Error |> next
                        | x -> x |> next)
                else
                    template
                    |> replaceQuasiquote context pos cont (n - 1) (Result.map (fun x' -> SUnquote x', pos) >> next)
            | Some(QqUnquoteSplicing template) ->
                if n = 0 then
                    EvalError("unquote-splicing must be in a list or vector context.", pos)
                    |> Error
                    |> next
                else
                    template
                    |> replaceQuasiquote
                        context
                        pos
                        cont
                        (n - 1)
                        (Result.map (fun x' -> SUnquoteSplicing x', pos) >> next)
            | Some(QqQuasiquote template) ->
                template
                |> replaceQuasiquote context pos cont (n + 1) (Result.map (fun x' -> SQuasiquote x', pos) >> next)
            | Some(QqQuote template) ->
                template
                |> replaceQuasiquote context pos cont n (Result.map (fun x' -> SQuote x', pos) >> next)
            | None -> arg |> Ok |> next

    let sQuasiquote context pos cont =
        function
        | [ template ] -> template |> replaceQuasiquote context pos cont 0 cont
        | x -> x |> invalidParameter pos "'%s' invalid quasiquote parameter." |> cont
