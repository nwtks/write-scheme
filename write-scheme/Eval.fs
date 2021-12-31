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

let rec sQuasiquote eval envs cont =
    let rec map acc =
        function
        | [] -> acc |> newList
        | x :: xs ->
            x
            |> quasiquote (fun a -> xs |> map (acc @ unquoteSplicing a))

    and quasiquote cont' =
        function
        | SList xs -> xs |> map [] |> cont'
        | SPair (xs, y) ->
            xs
            |> map []
            |> function
                | SList ys -> SPair(ys, unquote y) |> cont
        | x -> unquote x |> cont'

    and unquote =
        function
        | SUnquote x
        | SList [ SSymbol "unquote"; x ] -> x |> eval envs cont
        | SQuote x
        | SList [ SSymbol "quote"; x ] -> x |> quasiquote cont |> SQuote
        | x -> x

    and unquoteSplicing =
        function
        | SUnquote x
        | SList [ SSymbol "unquote"; x ] ->
            x
            |> eval envs cont
            |> function
                | SEmpty -> []
                | y -> [ y ]
        | SUnquoteSplicing x
        | SList [ SSymbol "unquote-splicing"; x ] ->
            x
            |> eval envs cont
            |> function
                | SEmpty -> []
                | SList xs -> xs
        | SQuote x
        | SList [ SSymbol "quote"; x ] -> [ x |> quasiquote cont |> SQuote ]
        | SEmpty -> []
        | x -> [ x ]

    quasiquote cont

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
    | SUnquote _
    | SUnquoteSplicing _
    | SClosure _
    | FFunction _ as x -> x |> cont
    | SQuote x -> x |> cont
    | SQuasiquote x -> x |> sQuasiquote eval envs cont
    | SSymbol x -> (lookupEnvs envs x).Value |> cont
    | SList [] -> SEmpty |> cont
    | SList [ SSymbol "quote"; x ] -> SQuote x |> eval envs cont
    | SList [ SSymbol "quasiquote"; x ] -> SQuasiquote x |> eval envs cont
    | SList [ SSymbol "unquote"; x ] -> SUnquote x |> eval envs cont
    | SList [ SSymbol "unquote-splicing"; x ] -> SUnquoteSplicing x |> eval envs cont
    | SList (operator :: operands) -> operator |> eval envs (proc operands)
