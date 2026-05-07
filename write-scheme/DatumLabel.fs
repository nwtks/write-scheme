namespace WriteScheme

open Type

module DatumLabel =
    [<TailCall>]
    let rec unwrapDatumLabel =
        function
        | SDatumLabel(_, d), _ -> unwrapDatumLabel d
        | e -> e

    [<TailCall>]
    let rec collectDatum labels =
        function
        | [] -> Ok labels
        | expression :: rest ->
            match expression with
            | SDatumLabel(n, _), pos when labels |> Map.containsKey n ->
                EvalError(sprintf "Duplicate datum label definition: #%d=" n, pos) |> Error
            | SDatumLabel(n, d), pos ->
                let labels' = labels |> Map.add n (unwrapDatumLabel d, pos)
                d :: rest |> collectDatum labels'
            | SPair p, _ -> p.car :: p.cdr :: rest |> collectDatum labels
            | SVector v, _ -> rest |> Array.foldBack (fun e s -> e :: s) v |> collectDatum labels
            | SRecord(_, _, fields), _ ->
                rest
                |> Array.foldBack (fun (f: SExpression ref) s -> f.Value :: s) fields
                |> collectDatum labels
            | SValues args, _
            | SError(_, args), _ -> args @ rest |> collectDatum labels
            | SQuote d, _
            | SQuasiquote d, _
            | SUnquote d, _
            | SUnquoteSplicing d, _ -> d :: rest |> collectDatum labels
            | _ -> rest |> collectDatum labels

    let isBefore =
        function
        | Some p1, Some p2 -> p1.Line < p2.Line || p1.Line = p2.Line && p1.Column < p2.Column
        | _ -> false

    [<TailCall>]
    let rec resolveLabel n pos labels visited =
        if visited |> Set.contains n then
            EvalError(sprintf "Invalid circular reference for datum label: #%d#" n, pos)
            |> Error
        else
            labels
            |> Map.tryFind n
            |> function
                | Some(_, defPos) when isBefore (pos, defPos) ->
                    EvalError(sprintf "Invalid forward reference for datum label: #%d#" n, pos)
                    |> Error
                | Some(v, _) ->
                    match v with
                    | SDatumRef m, refPos -> resolveLabel m refPos labels (visited |> Set.add n)
                    | _ -> Ok v
                | None -> EvalError(sprintf "Undefined datum label: #%d#" n, pos) |> Error

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
        | [] -> acc |> List.rev |> next
        | x :: xs ->
            x
            |> resolveDatumRefBind labels (fun r -> xs |> resolveDatumRefList labels (r :: acc) next)

    let resolveLabels expression =
        [ expression ]
        |> collectDatum Map.empty
        |> Result.bind (fun labels -> expression |> resolveDatumRef labels id)
