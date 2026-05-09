namespace WriteScheme

open Type

module Eval =
    [<TailCall>]
    let rec apply envs cont args =
        function
        | SParameter(param, converterOpt), pos ->
            match args with
            | [] -> param.Value |> Ok |> cont
            | [ v ] ->
                match converterOpt with
                | Some converter ->
                    converter
                    |> apply
                        envs
                        (Result.map (fun converted ->
                            param.Value <- converted
                            converted)
                         >> cont)
                        [ v ]
                | None ->
                    param.Value <- v
                    v |> Ok |> cont
            | _ ->
                EvalError(sprintf "'%s' invalid parameter object call." (args |> toSPair |> Print.print), pos)
                |> Error
        | SSyntax fn, pos
        | SProcedure fn, pos -> args |> fn envs pos cont
        | SContinuation fn, pos ->
            match args with
            | [ arg ] -> Ok arg |> fn
            | _ ->
                EvalError(sprintf "'%s' invalid continuation parameter." (args |> toSPair |> Print.print), pos)
                |> Error
        | x -> EvalError(sprintf "'%s' not operator." (x |> Print.print), snd x) |> Error

    [<TailCall>]
    let rec eval envs cont =
        function
        | SEmpty, pos -> EvalError("() is not a valid expression.", pos) |> Error
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
        | SSymbol x, pos -> x |> Context.lookupEnvs envs pos |> Result.map (fun v -> v.Value) |> cont
        | SPair p, _ ->
            p.car
            |> eval envs (function
                | Ok(SSyntax fn, pos') ->
                    p.cdr
                    |> toList
                    |> function
                        | Ok args -> args |> fn envs pos' cont
                        | Error e -> Error e |> cont
                | Ok op ->
                    p.cdr
                    |> toList
                    |> function
                        | Ok args -> args |> evalArgs envs cont (fun e c a -> op |> apply e c a) []
                        | Error e -> Error e |> cont
                | x -> x |> cont)
        | SQuote x, pos -> [ SSymbol "quote", pos; x ] |> toSPair |> eval envs cont
        | SQuasiquote x, pos -> [ SSymbol "quasiquote", pos; x ] |> toSPair |> eval envs cont

    and [<TailCall>] evalArgs envs cont fn acc =
        function
        | [] -> acc |> List.rev |> fn envs cont
        | x :: xs ->
            x
            |> eval envs (function
                | Ok(SValues _, p) -> EvalError("Multiple values in single value context.", p) |> Error |> cont
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
