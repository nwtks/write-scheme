namespace WriteScheme

open Type

module DatumLabel =
    [<TailCall>]
    let rec unwrapDatumLabel =
        function
        | SDatumLabel(_, d), _ -> unwrapDatumLabel d
        | e -> e

    let collectChildren =
        function
        | SPair p, _ -> [ p.car; p.cdr ]
        | SVector v, _ -> v |> Array.toList
        | SRecord(_, _, fields), _ -> fields |> Array.toList |> List.map (fun f -> f.Value)
        | SValues args, _
        | SError(_, args), _ -> args
        | SQuote d, _
        | SQuasiquote d, _
        | SUnquote d, _
        | SUnquoteSplicing d, _ -> [ d ]
        | _ -> []

    [<TailCall>]
    let rec collectDatum labels =
        function
        | [] -> Ok labels
        | (SDatumLabel(n, _), pos) :: _ when labels |> Map.containsKey n ->
            EvalError($"Duplicate datum label definition: #{n}=", pos) |> Error
        | (SDatumLabel(n, d), pos) :: rest ->
            let labels' = labels |> Map.add n (unwrapDatumLabel d, pos)
            d :: rest |> collectDatum labels'
        | expression :: rest -> collectChildren expression @ rest |> collectDatum labels

    let isBefore =
        function
        | Some p1, Some p2 -> p1.line < p2.line || p1.line = p2.line && p1.column < p2.column
        | _ -> false

    [<TailCall>]
    let rec resolveLabel n pos labels visited =
        if visited |> Set.contains n then
            EvalError($"Invalid circular reference for datum label: #{n}#", pos) |> Error
        else
            labels
            |> Map.tryFind n
            |> function
                | Some(_, defPos) when isBefore (pos, defPos) ->
                    EvalError($"Invalid forward reference for datum label: #{n}#", pos) |> Error
                | Some(v, _) ->
                    match v with
                    | SDatumRef m, refPos -> resolveLabel m refPos labels (visited |> Set.add n)
                    | _ -> Ok v
                | None -> EvalError($"Undefined datum label: #{n}#", pos) |> Error

    [<TailCall>]
    let rec resolveDatumRef labels next =
        function
        | SDatumLabel(_, d), _ -> d |> resolveDatumRef labels next
        | SDatumRef n, pos -> resolveLabel n pos labels Set.empty |> next
        | SPair p, pos -> resolveDatumRefPair labels p pos next
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
        | SQuote d, pos -> resolveQuoteLike labels pos next SQuote d
        | SQuasiquote d, pos -> resolveQuoteLike labels pos next SQuasiquote d
        | SUnquote d, pos -> resolveQuoteLike labels pos next SUnquote d
        | SUnquoteSplicing d, pos -> resolveQuoteLike labels pos next SUnquoteSplicing d
        | x -> Ok x |> next

    and [<TailCall>] resolveQuoteLike labels pos next wrap expression =
        expression
        |> resolveDatumRef labels (Result.bind (fun x -> Ok(wrap x, pos) |> next))

    and [<TailCall>] resolveDatumRefPair labels pair pos next =
        pair.car
        |> resolveDatumRef
            labels
            (Result.bind (fun car ->
                pair.car <- car

                pair.cdr
                |> resolveDatumRef
                    labels
                    (Result.bind (fun cdr ->
                        pair.cdr <- cdr
                        Ok(SPair pair, pos) |> next))))

    and [<TailCall>] resolveDatumRefArray labels i len get set next =
        if i = len then
            next ()
        else
            get i
            |> resolveDatumRef
                labels
                (Result.bind (fun r ->
                    set i r
                    resolveDatumRefArray labels (i + 1) len get set next))

    and [<TailCall>] resolveDatumRefList labels acc next =
        function
        | [] -> acc |> List.rev |> next
        | x :: xs ->
            x
            |> resolveDatumRef labels (Result.bind (fun r -> xs |> resolveDatumRefList labels (r :: acc) next))

    let resolveLabels expression =
        [ expression ]
        |> collectDatum Map.empty
        |> Result.bind (fun labels -> expression |> resolveDatumRef labels id)
