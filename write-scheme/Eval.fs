module Eval

open Type
open Print

let extendEnvs envs bindings = (Map.ofList bindings |> SEnv) :: envs

let lookupEnvs envs symbol =
    let lookup (env: SEnv) =
        match env.TryGetValue symbol with
        | true, x -> Some x
        | _ -> None

    match List.tryPick lookup envs with
    | Some x -> x
    | None -> sprintf "No binding for '%s'." symbol |> failwith

let rec eval envs cont =
    function
    | SEmpty
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
    | SContinuation _ as x -> x |> cont
    | SSymbol x -> (lookupEnvs envs x).Value |> cont
    | SQuote x -> SList [ SSymbol "quote"; x ] |> eval envs cont
    | SQuasiquote x ->
        SList [ SSymbol "quasiquote"; x ]
        |> eval envs cont
    | SList [] -> SEmpty |> cont
    | SList (operator :: operands) -> operator |> eval envs (apply envs cont operands)

and apply envs cont args =
    let rec mapEval fn acc =
        function
        | [] -> List.rev acc |> fn envs cont
        | x :: xs ->
            x
            |> eval envs (fun a -> xs |> mapEval fn (a :: acc))

    function
    | SSyntax fn -> fn envs cont args
    | SProcedure fn -> mapEval fn [] args
    | SContinuation fn ->
        match args with
        | [ arg ] -> fn arg
        | _ ->
            args
            |> newList
            |> print
            |> sprintf "'%s' invalid continuation parameter."
            |> failwith
    | x ->
        print x
        |> sprintf "'%s' not operator."
        |> failwith
