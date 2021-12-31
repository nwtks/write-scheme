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

let sQuote eval envs cont =
    let rec unquote cont' =
        function
        | SUnquote x
        | SList [ SSymbol "unquote"; x ] -> x |> eval envs cont'
        | SList xs -> xs |> map [] |> cont'
        | x -> x |> cont'

    and map acc =
        function
        | [] -> List.rev acc |> newList
        | x :: xs -> x |> unquote (fun a -> xs |> map (a :: acc))

    unquote cont

let rec eval envs cont =
    let rec map fn acc =
        function
        | [] -> List.rev acc |> fn cont
        | x :: xs -> x |> eval envs (fun a -> xs |> map fn (a :: acc))

    let proc xs =
        function
        | SClosure fn -> fn envs cont xs
        | FFunction fn -> map fn [] xs
        | x ->
            print x
            |> sprintf "'%s' not operator."
            |> failwith

    function
    | SEmpty
    | SBool _
    | SRational _
    | SReal _
    | SString _
    | SChar _
    | SPair _
    | SQuasiquote _
    | SUnquote _
    | SUnquoteSplicing _
    | SClosure _
    | FFunction _ as x -> x |> cont
    | SSymbol x -> (lookupEnvs envs x).Value |> cont
    | SQuote x -> sQuote eval envs cont x
    | SList [] -> SEmpty |> cont
    | SList [ SSymbol "quote"; x ] -> SQuote x |> eval envs cont
    | SList [ SSymbol "quasiquote"; x ] -> SQuasiquote x |> eval envs cont
    | SList [ SSymbol "unquote"; x ] -> SUnquote x |> eval envs cont
    | SList [ SSymbol "unquote-splicing"; x ] -> SUnquoteSplicing x |> eval envs cont
    | SList (operator :: operands) -> operator |> eval envs (proc operands)
