namespace WriteScheme

open Type

module Eval =
    let extendEnvs envs bindings = (Map.ofList bindings |> SEnv) :: envs

    let defineEnvVar (envs: SEnv list) var value =
        if envs.Head.ContainsKey var then
            envs.Head.[var].Value <- value
        else
            envs.Head.Add(var, ref value)

    let tryLookupEnvs envs symbol =
        let lookup (env: SEnv) =
            match env.TryGetValue symbol with
            | true, x -> Some x
            | _ -> None

        List.tryPick lookup envs

    let lookupEnvs envs symbol =
        match tryLookupEnvs envs symbol with
        | Some x -> x
        | None -> sprintf "No binding for '%s'." symbol |> failwith

    [<TailCall>]
    let rec matchEval envs cont =
        function
        | SEmpty -> failwith "() is not a valid expression. It must be quoted."
        | SUnspecified
        | SBool _
        | SRational _
        | SReal _
        | SString _
        | SChar _
        | SPair _
        | SVector _
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
        | SSymbol x -> (lookupEnvs envs x).Value |> cont
        | SList [] -> SEmpty |> cont
        | SList(operator :: operands) -> operator |> matchEval envs (apply envs cont operands)
        | SQuote x -> SList [ SSymbol "quote"; x ] |> matchEval envs cont
        | SQuasiquote x -> SList [ SSymbol "quasiquote"; x ] |> matchEval envs cont

    and [<TailCall>] evalArgs envs cont fn acc =
        function
        | [] -> List.rev acc |> fn envs cont
        | x :: xs -> x |> matchEval envs (fun a -> xs |> evalArgs envs cont fn (a :: acc))

    and [<TailCall>] apply envs cont args =
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
        | SSyntax fn -> fn envs cont args
        | SProcedure fn -> evalArgs envs cont fn [] args
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

    let eval envs cont expr =
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
            matchEval envs cont expr
        with e ->
            processErr e

    [<TailCall>]
    let rec eachEval envs cont acc =
        function
        | [] -> acc |> cont
        | x :: xs -> x |> eval envs (fun a -> xs |> eachEval envs cont a)
