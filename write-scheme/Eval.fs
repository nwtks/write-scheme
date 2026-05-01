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
        | SContinuation _, _ as expr -> expr |> cont
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
