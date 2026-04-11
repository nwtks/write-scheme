namespace WriteScheme

open Type

module Eval =
    let extendEnvs envs bindings = (Map.ofList bindings |> SEnv) :: envs

    let lookupEnvs envs symbol =
        let lookup (env: SEnv) =
            match env.TryGetValue symbol with
            | true, x -> Some x
            | _ -> None

        match List.tryPick lookup envs with
        | Some x -> x
        | None -> sprintf "No binding for '%s'." symbol |> failwith

    [<TailCall>]
    let rec matchEval envs cont expr =
        match expr with
        | SEmpty -> failwith "() is not a valid expression. It must be quoted."
        | SUnspecified
        | SBool _
        | SRational _
        | SReal _
        | SString _
        | SChar _
        | SPair _
        | SUnquote _
        | SUnquoteSplicing _
        | SSyntax _
        | SProcedure _
        | SContinuation _
        | SError _ as x -> x |> cont
        | SSymbol x -> (lookupEnvs envs x).Value |> cont
        | SQuote x -> SList [ SSymbol "quote"; x ] |> matchEval envs cont
        | SQuasiquote x -> SList [ SSymbol "quasiquote"; x ] |> matchEval envs cont
        | SList [] -> SEmpty |> cont
        | SList(operator :: operands) -> operator |> matchEval envs (apply envs cont operands)

    and mapEval envs cont fn acc =
        function
        | [] -> List.rev acc |> fn envs cont
        | x :: xs -> x |> matchEval envs (fun a -> xs |> mapEval envs cont fn (a :: acc))

    and apply envs cont args =

        function
        | SSyntax fn -> fn envs cont args
        | SProcedure fn -> mapEval envs cont fn [] args
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
