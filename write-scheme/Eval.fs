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

let sQuasiquote eval envs cont =
    let cons x y =
        match y with
        | SEmpty -> [ x ]
        | SList ys -> x :: ys
        | _ -> [ x; y ]

    let join x y =
        match x, y with
        | SEmpty, SEmpty -> []
        | SList xs, SEmpty -> xs
        | SEmpty, SList ys -> ys
        | SList xs, SList ys -> xs @ ys
        | _ -> [ x; y ]

    let rec replace n =
        function
        | SEmpty -> SEmpty
        | SList xs -> replaceList n xs |> SList
        | SPair (x1, x2) -> SPair(replaceList n x1, replaceDatum n x2)
        | x -> replaceDatum n x

    and replaceList n =
        function
        | [] -> []
        | SUnquote x :: xs
        | SList [ SSymbol "unquote"; x ] :: xs ->
            if n = 0 then
                cons (x |> eval envs cont) (xs |> newList |> replace n)
            else
                cons (x |> replace (n - 1) |> SUnquote) (xs |> newList |> replace n)
        | SUnquoteSplicing x :: xs
        | SList [ SSymbol "unquote-splicing"; x ] :: xs ->
            if n = 0 then
                join (x |> eval envs cont) (xs |> newList |> replace n)
            else
                cons (x |> replace (n - 1) |> SUnquoteSplicing) (xs |> newList |> replace n)
        | SQuasiquote x :: xs
        | SList [ SSymbol "quasiquote"; x ] :: xs ->
            cons (x |> replace (n + 1) |> SQuasiquote) (xs |> newList |> replace n)
        | SQuote x :: xs
        | SList [ SSymbol "quote"; x ] :: xs -> cons (x |> replace n |> SQuote) (xs |> newList |> replace n)
        | x :: xs -> cons (x |> replace n) (xs |> newList |> replace n)

    and replaceDatum n =
        function
        | SUnquote x
        | SList [ SSymbol "unquote"; x ] ->
            if n = 0 then
                x |> eval envs cont
            else
                x |> replace (n - 1) |> SUnquote
        | SUnquoteSplicing x
        | SList [ SSymbol "unquote-splicing"; x ] ->
            if n = 0 then
                x |> eval envs cont
            else
                x |> replace (n - 1) |> SUnquoteSplicing
        | SQuasiquote x
        | SList [ SSymbol "quasiquote"; x ] -> x |> replace (n + 1) |> SQuasiquote
        | SQuote x
        | SList [ SSymbol "quote"; x ] -> x |> replace n |> SQuote
        | x -> x

    replace 0

let rec eval envs cont =
    let rec mapEval fn acc =
        function
        | [] -> List.rev acc |> fn envs cont
        | x :: xs ->
            x
            |> eval envs (fun a -> xs |> mapEval fn (a :: acc))

    let proc args =
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
    | SList [] -> SEmpty |> cont
    | SQuote x
    | SList [ SSymbol "quote"; x ] -> x |> cont
    | SQuasiquote x
    | SList [ SSymbol "quasiquote"; x ] -> x |> sQuasiquote eval envs cont
    | SList (operator :: operands) -> operator |> eval envs (proc operands)
