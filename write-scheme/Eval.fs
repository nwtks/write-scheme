namespace WriteScheme

open Type

module Eval =
    [<TailCall>]
    let rec apply envs cont args =
        function
        | SParameter(r, converterOpt), pos ->
            match args with
            | [] -> r.Value |> Ok |> cont
            | [ v ] ->
                match converterOpt with
                | Some converter ->
                    converter
                    |> apply
                        envs
                        (Result.map (fun converted ->
                            r.Value <- converted
                            converted)
                         >> cont)
                        [ v ]
                | None ->
                    r.Value <- v
                    v |> Ok |> cont
            | _ -> Error(EvalError(sprintf "'%s' invalid parameter object call." (args |> toSPair |> Print.print), pos))
        | SSyntax fn, pos
        | SProcedure fn, pos -> args |> fn envs pos cont
        | SContinuation fn, pos ->
            match args with
            | [ arg ] -> fn (Ok arg)
            | _ ->
                Error(EvalError(sprintf "'%s' invalid continuation parameter." (args |> toSPair |> Print.print), pos))
        | x -> Error(EvalError(sprintf "'%s' not operator." (x |> Print.print), snd x))

    [<TailCall>]
    let rec eval envs cont =
        function
        | SEmpty, pos -> Error(EvalError("() is not a valid expression.", pos))
        | SUnspecified, _
        | SBool _, _
        | SRational _, _
        | SReal _, _
        | SComplex _, _
        | SString _, _
        | SChar _, _
        | SVector _, _
        | SByteVector _, _
        | SValues _, _
        | SRecord _, _
        | SError _, _
        | SUnquote _, _
        | SUnquoteSplicing _, _
        | SPromise _, _
        | SParameter _, _
        | SSyntax _, _
        | SProcedure _, _
        | SContinuation _, _
        | SDatumLabel _, _
        | SDatumRef _, _ as expr -> expr |> Ok |> cont
        | SSymbol x, pos -> Context.lookupEnvs envs pos x |> Result.map (fun v -> v.Value) |> cont
        | SPair p, _ ->
            p.car
            |> eval envs (function
                | Ok(SSyntax fn, pos') ->
                    match p.cdr |> toList with
                    | Ok args -> args |> fn envs pos' cont
                    | Error e -> Error e |> cont
                | Ok op ->
                    match p.cdr |> toList with
                    | Ok args -> args |> evalArgs envs cont (fun e c a -> op |> apply e c a) []
                    | Error e -> Error e |> cont
                | x -> x |> cont)
        | SQuote x, pos -> [ SSymbol "quote", pos; x ] |> toSPair |> eval envs cont
        | SQuasiquote x, pos -> [ SSymbol "quasiquote", pos; x ] |> toSPair |> eval envs cont

    and [<TailCall>] evalArgs envs cont fn acc =
        function
        | [] -> List.rev acc |> fn envs cont
        | x :: xs ->
            x
            |> eval envs (function
                | Ok a -> xs |> evalArgs envs cont fn (a :: acc)
                | Error e -> Error e |> cont)

    [<TailCall>]
    let rec eachEval envs cont acc =
        function
        | [] -> acc |> cont
        | x :: xs ->
            x
            |> eval envs (function
                | Ok a -> xs |> eachEval envs cont (Ok a)
                | Error e -> Error e |> cont)

    [<TailCall>]
    let rec unwrapDatumLabel =
        function
        | SDatumLabel(_, d), _ -> unwrapDatumLabel d
        | e -> e

    [<TailCall>]
    let rec collectDatum labels =
        function
        | [] -> Ok labels
        | e :: rest ->
            match e with
            | SDatumLabel(n, d), pos ->
                if Map.containsKey n labels then
                    Error(EvalError(sprintf "Duplicate datum label definition: #%d=" n, pos))
                else
                    let labels' = Map.add n (unwrapDatumLabel d, pos) labels
                    d :: rest |> collectDatum labels'
            | SPair p, _ -> p.car :: p.cdr :: rest |> collectDatum labels
            | SVector v, _ -> Array.foldBack (fun e s -> e :: s) v rest |> collectDatum labels
            | SRecord(_, _, fields), _ ->
                Array.foldBack (fun (f: SExpression ref) s -> f.Value :: s) fields rest
                |> collectDatum labels
            | SError(_, args), _
            | SValues args, _ -> args @ rest |> collectDatum labels
            | SQuote d, _
            | SQuasiquote d, _
            | SUnquote d, _
            | SUnquoteSplicing d, _ -> d :: rest |> collectDatum labels
            | _ -> rest |> collectDatum labels

    let isBefore pos1 pos2 =
        match pos1, pos2 with
        | Some p1, Some p2 -> p1.Line < p2.Line || p1.Line = p2.Line && p1.Column < p2.Column
        | _ -> false

    [<TailCall>]
    let rec resolveLabel n pos labels visited =
        if Set.contains n visited then
            Error(EvalError(sprintf "Invalid circular reference for datum label: #%d#" n, pos))
        else
            match Map.tryFind n labels with
            | None -> Error(EvalError(sprintf "Undefined datum label: #%d#" n, pos))
            | Some(v, defPos) ->
                if isBefore pos defPos then
                    Error(EvalError(sprintf "Invalid forward reference for datum label: #%d#" n, pos))
                else
                    match v with
                    | SDatumRef m, refPos -> resolveLabel m refPos labels (Set.add n visited)
                    | _ -> Ok v

    [<TailCall>]
    let rec resolveDatumRef labels next =
        function
        | SDatumLabel(_, d), _ -> d |> resolveDatumRef labels next
        | SDatumRef n, pos -> resolveLabel n pos labels Set.empty |> next
        | SPair p, pos ->
            p.car
            |> resolveDatumRefBind labels (fun car ->
                p.car <- car

                p.cdr
                |> resolveDatumRefBind labels (fun cdr ->
                    p.cdr <- cdr
                    Ok(SPair p, pos) |> next))

        | SVector v, pos ->
            resolveDatumRefArray labels 0 v.Length (fun i -> v.[i]) (fun i r -> v.[i] <- r) (fun () ->
                Ok(SVector v, pos) |> next)
        | SRecord(id, name, fields), pos ->
            resolveDatumRefArray
                labels
                0
                fields.Length
                (fun i -> fields.[i].Value)
                (fun i r -> fields.[i].Value <- r)
                (fun () -> Ok(SRecord(id, name, fields), pos) |> next)
        | SError(msg, args), pos ->
            args
            |> resolveDatumRefList labels [] (fun resolved -> Ok(SError(msg, resolved), pos) |> next)
        | SValues args, pos ->
            args
            |> resolveDatumRefList labels [] (fun resolved -> Ok(SValues resolved, pos) |> next)
        | SQuote d, pos -> d |> resolveDatumRefBind labels (fun x -> Ok(SQuote x, pos) |> next)
        | SQuasiquote d, pos -> d |> resolveDatumRefBind labels (fun x -> Ok(SQuasiquote x, pos) |> next)
        | SUnquote d, pos -> d |> resolveDatumRefBind labels (fun x -> Ok(SUnquote x, pos) |> next)
        | SUnquoteSplicing d, pos -> d |> resolveDatumRefBind labels (fun x -> Ok(SUnquoteSplicing x, pos) |> next)
        | x -> Ok x |> next

    and resolveDatumRefBind labels next data =
        data |> resolveDatumRef labels (Result.bind next)

    and [<TailCall>] resolveDatumRefArray labels i len get set next =
        if i = len then
            next ()
        else
            get i
            |> resolveDatumRefBind labels (fun r ->
                set i r
                resolveDatumRefArray labels (i + 1) len get set next)

    and [<TailCall>] resolveDatumRefList labels acc next =
        function
        | [] -> List.rev acc |> next
        | x :: xs ->
            x
            |> resolveDatumRefBind labels (fun r -> xs |> resolveDatumRefList labels (r :: acc) next)

    let resolveLabels expr =
        [ expr ]
        |> collectDatum Map.empty
        |> Result.bind (fun labels -> expr |> resolveDatumRef labels id)
