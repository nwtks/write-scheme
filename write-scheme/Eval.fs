module Eval

open Type

let extendEnvs envs bindings = (Map.ofList bindings |> SEnv) :: envs

let lookupEnvs envs symbol =
    match
        List.tryPick
            (fun (env: SEnv) ->
                match env.TryGetValue symbol with
                | true, v -> Some v
                | _ -> None)
            envs
        with
    | Some v -> v
    | None -> sprintf "No binding for '%s'." symbol |> failwith

let sQuote eval envs cont =
    let rec unquote cont' =
        function
        | SUnquote e
        | SList [ SSymbol "unquote"; e ] -> e |> eval envs cont'
        | SList xs -> xs |> map [] |> cont'
        | e -> e |> cont'

    and map acc =
        function
        | [] -> List.rev acc |> SList
        | x' :: xs' -> x' |> unquote (fun a -> xs' |> map (a :: acc))

    unquote cont

let rec eval envs cont =
    let rec map fn acc =
        function
        | [] -> List.rev acc |> fn cont
        | x :: xs -> x |> eval envs (fun a -> xs |> map fn (a :: acc))

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
    | FFunction _ as e -> e |> cont
    | SSymbol s -> (lookupEnvs envs s).Value |> cont
    | SQuote datum -> sQuote eval envs cont datum
    | SList [] -> SEmpty |> cont
    | SList [ SSymbol "quote"; datum ] -> SQuote datum |> eval envs cont
    | SList [ SSymbol "quasiquote"; datum ] -> SQuasiquote datum |> eval envs cont
    | SList [ SSymbol "unquote"; datum ] -> SUnquote datum |> eval envs cont
    | SList [ SSymbol "unquote-splicing"; datum ] -> SUnquoteSplicing datum |> eval envs cont
    | SList (operator :: operands) ->
        operator
        |> eval envs (function
            | SClosure fn -> fn envs cont operands
            | FFunction fn -> map fn [] operands)
