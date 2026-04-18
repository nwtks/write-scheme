namespace WriteScheme

open Type

module Eval =
    [<TailCall>]
    let rec apply envs cont args =
        function
        | SParameter(r, converterOpt) ->
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
                args
                |> toSList
                |> Print.print
                |> sprintf "'%s' invalid parameter object call."
                |> failwith
        | SSyntax fn
        | SProcedure fn -> args |> fn envs cont
        | SContinuation fn ->
            match args with
            | [ arg ] -> fn arg
            | _ ->
                args
                |> toSList
                |> Print.print
                |> sprintf "'%s' invalid continuation parameter."
                |> failwith
        | x -> Print.print x |> sprintf "'%s' not operator." |> failwith

    [<TailCall>]
    let rec eval envs cont =
        function
        | SEmpty -> failwith "() is not a valid expression. It must be quoted."
        | SUnspecified
        | SBool _
        | SRational _
        | SReal _
        | SComplex _
        | SString _
        | SChar _
        | SPair _
        | SVector _
        | SByteVector _
        | SValues _
        | SRecord _
        | SError _
        | SUnquote _
        | SUnquoteSplicing _
        | SPromise _
        | SParameter _
        | SSyntax _
        | SProcedure _
        | SContinuation _ as x -> x |> cont
        | SSymbol x -> (Context.lookupEnvs envs x).Value |> cont
        | SList [] -> SEmpty |> cont
        | SList(operator :: operands) ->
            operator
            |> eval envs (function
                | SSyntax fn -> fn envs cont operands
                | _ as op -> operands |> evalArgs envs cont (fun e c a -> op |> apply e c a) [])
        | SQuote x -> [ SSymbol "quote"; x ] |> toSList |> eval envs cont
        | SQuasiquote x -> [ SSymbol "quasiquote"; x ] |> toSList |> eval envs cont

    and [<TailCall>] evalArgs envs cont fn acc =
        function
        | [] -> List.rev acc |> fn envs cont
        | x :: xs -> x |> eval envs (fun a -> xs |> evalArgs envs cont fn (a :: acc))

    [<TailCall>]
    let rec eachEval envs cont acc =
        function
        | [] -> acc |> cont
        | x :: xs -> x |> eval envs (fun a -> xs |> eachEval envs cont a)

    let evalWrapped envs cont expr =
        let processErr (e: exn) =
            match e with
            | :? SchemeRaise -> raise e
            | _ ->
                if e.Message.Contains " (at line " then
                    raise e
                else
                    match Read.getExprPos expr with
                    | Some pos -> failwithf "%s (at line %d, column %d)" e.Message (int pos.Line) (int pos.Column)
                    | None -> raise e

        try
            eval envs cont expr
        with e ->
            processErr e
