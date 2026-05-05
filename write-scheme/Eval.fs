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
        | SDatumLabel _, _
        | SDatumRef _, _
        | SPromise _, _
        | SParameter _, _
        | SSyntax _, _
        | SProcedure _, _
        | SContinuation _, _ as expr -> expr |> Ok |> cont
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
                | x -> x |> cont)

    [<TailCall>]
    let rec eachEval envs cont acc =
        function
        | [] -> acc |> cont
        | x :: xs ->
            x
            |> eval envs (function
                | Ok a -> xs |> eachEval envs cont (Ok a)
                | x -> x |> cont)
