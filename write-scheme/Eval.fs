namespace WriteScheme

open Type

module Eval =
    [<TailCall>]
    let rec apply envs cont args =
        function
        | SParameter(r, converterOpt), pos ->
            match args with
            | [] -> r.Value |> cont
            | [ v ] ->
                match converterOpt with
                | Some converter ->
                    converter
                    |> apply
                        envs
                        (fun converted ->
                            r.Value <- converted
                            converted |> cont)
                        [ v ]
                | None ->
                    r.Value <- v
                    v |> cont
            | _ ->
                failwithf
                    "'%s' invalid parameter object call.%s"
                    (args |> toSPair |> Print.print)
                    (pos |> formatPosition)
        | SSyntax fn, pos
        | SProcedure fn, pos -> args |> fn envs pos cont
        | SContinuation fn, pos ->
            match args with
            | [ arg ] -> fn arg
            | _ ->
                failwithf
                    "'%s' invalid continuation parameter.%s"
                    (args |> toSPair |> Print.print)
                    (pos |> formatPosition)
        | x -> failwithf "'%s' not operator.%s" (x |> Print.print) (x |> snd |> formatPosition)

    [<TailCall>]
    let rec eval envs cont =
        function
        | SEmpty, pos -> failwithf "() is not a valid expression. %s" (pos |> formatPosition)
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
        | SDatumRef _, _ as expr -> expr |> cont
        | SSymbol x, pos -> (Context.lookupEnvs envs pos x).Value |> cont
        | SPair p, _ ->
            p.car
            |> eval envs (function
                | SSyntax fn, pos' -> p.cdr |> toList |> fn envs pos' cont
                | op -> p.cdr |> toList |> evalArgs envs cont (fun e c a -> op |> apply e c a) [])
        | SQuote x, pos -> [ SSymbol "quote", pos; x ] |> toSPair |> eval envs cont
        | SQuasiquote x, pos -> [ SSymbol "quasiquote", pos; x ] |> toSPair |> eval envs cont

    and [<TailCall>] evalArgs envs cont fn acc =
        function
        | [] -> List.rev acc |> fn envs cont
        | x :: xs -> x |> eval envs (fun a -> xs |> evalArgs envs cont fn (a :: acc))

    [<TailCall>]
    let rec eachEval envs cont acc =
        function
        | [] -> acc |> cont
        | x :: xs -> x |> eval envs (fun a -> xs |> eachEval envs cont a)

    [<TailCall>]
    let rec unwrapDatumLabel =
        function
        | SDatumLabel(_, d), _ -> unwrapDatumLabel d
        | e -> e

    [<TailCall>]
    let rec collectDatum labels =
        function
        | [] -> labels
        | e :: rest ->
            match e with
            | SDatumLabel(n, d), pos ->
                if Map.containsKey n labels then
                    failwithf "Duplicate datum label definition: #%d=%s" n (pos |> formatPosition)

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
            failwithf "Invalid circular reference for datum label: #%d#%s" n (pos |> formatPosition)

        match Map.tryFind n labels with
        | None -> failwithf "Undefined datum label: #%d#%s" n (pos |> formatPosition)
        | Some(v, defPos) ->
            if isBefore pos defPos then
                failwithf "Invalid forward reference for datum label: #%d#%s" n (pos |> formatPosition)

            match v with
            | SDatumRef m, refPos -> resolveLabel m refPos labels (Set.add n visited)
            | _ -> v

    [<TailCall>]
    let rec resolveDatumRef labels next =
        function
        | SDatumLabel(_, d), _ -> d |> resolveDatumRef labels next
        | SDatumRef n, pos -> resolveLabel n pos labels Set.empty |> next
        | SPair p, pos ->
            p.car
            |> resolveDatumRef labels (fun car ->
                p.car <- car

                p.cdr
                |> resolveDatumRef labels (fun cdr ->
                    p.cdr <- cdr
                    (SPair p, pos) |> next))

        | SVector v, pos ->
            resolveDatumRefArray labels 0 v.Length (fun i -> v.[i]) (fun i r -> v.[i] <- r) (fun () ->
                (SVector v, pos) |> next)
        | SRecord(id, name, fields), pos ->
            resolveDatumRefArray
                labels
                0
                fields.Length
                (fun i -> fields.[i].Value)
                (fun i r -> fields.[i].Value <- r)
                (fun () -> (SRecord(id, name, fields), pos) |> next)
        | SError(msg, args), pos ->
            args
            |> resolveDatumRefList labels [] (fun resolved -> (SError(msg, resolved), pos) |> next)
        | SValues args, pos ->
            args
            |> resolveDatumRefList labels [] (fun resolved -> (SValues resolved, pos) |> next)
        | SQuote d, pos -> d |> resolveDatumRef labels (fun x -> (SQuote x, pos) |> next)
        | SQuasiquote d, pos -> d |> resolveDatumRef labels (fun x -> (SQuasiquote x, pos) |> next)
        | SUnquote d, pos -> d |> resolveDatumRef labels (fun x -> (SUnquote x, pos) |> next)
        | SUnquoteSplicing d, pos -> d |> resolveDatumRef labels (fun x -> (SUnquoteSplicing x, pos) |> next)
        | x -> x |> next

    and [<TailCall>] resolveDatumRefArray labels i len get set next =
        if i = len then
            next ()
        else
            get i
            |> resolveDatumRef labels (fun r ->
                set i r
                resolveDatumRefArray labels (i + 1) len get set next)

    and [<TailCall>] resolveDatumRefList labels acc next =
        function
        | [] -> List.rev acc |> next
        | x :: xs ->
            x
            |> resolveDatumRef labels (fun r -> xs |> resolveDatumRefList labels (r :: acc) next)

    let resolveLabels expr =
        let labels = [ expr ] |> collectDatum Map.empty
        expr |> resolveDatumRef labels id
