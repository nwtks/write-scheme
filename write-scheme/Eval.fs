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
    let cons x =
        function
        | SEmpty -> [ x ] |> newList
        | SList ys -> x :: ys |> newList
        | y -> [ x; y ] |> newList

    let join =
        function
        | SEmpty, SEmpty -> SEmpty
        | SList xs, SEmpty -> xs |> newList
        | SEmpty, SList ys -> ys |> newList
        | SList xs, SList ys -> xs @ ys |> newList
        | x, y -> [ x; y ] |> newList

    let rec replace n next =
        function
        | SEmpty -> SEmpty |> next
        | SList xs -> replaceList n xs |> next
        | SPair (x1, x2) ->
            match replaceList n x1 with
            | SList ys -> SPair(ys, replaceDatum n x2) |> next
            | _ -> replaceDatum n x2 |> next
        | x -> replaceDatum n x |> next

    and replaceList n =
        function
        | [] -> SEmpty
        | SUnquote x :: xs
        | SList [ SSymbol "unquote"; x ] :: xs ->
            if n = 0 then
                x
                |> eval envs (fun a ->
                    xs
                    |> newList
                    |> replace n (fun b -> cons a b |> cont))
            else
                x
                |> replace (n - 1) (fun a ->
                    xs
                    |> newList
                    |> replace n (fun b -> cons (SUnquote a) b))
        | SUnquoteSplicing x :: xs
        | SList [ SSymbol "unquote-splicing"; x ] :: xs ->
            if n = 0 then
                x
                |> eval envs (fun a ->
                    xs
                    |> newList
                    |> replace n (fun b -> join (a, b) |> cont))
            else
                x
                |> replace (n - 1) (fun a ->
                    xs
                    |> newList
                    |> replace n (fun b -> cons (SUnquoteSplicing a) b))
        | SQuasiquote x :: xs
        | SList [ SSymbol "quasiquote"; x ] :: xs ->
            x
            |> replace (n + 1) (fun a ->
                xs
                |> newList
                |> replace n (fun b -> cons (SQuasiquote a) b))
        | SQuote x :: xs
        | SList [ SSymbol "quote"; x ] :: xs ->
            x
            |> replace n (fun a ->
                xs
                |> newList
                |> replace n (fun b -> cons (SQuote a) b))
        | x :: xs ->
            x
            |> replace n (fun a -> xs |> newList |> replace n (fun b -> cons a b))

    and replaceDatum n =
        function
        | SUnquote x
        | SList [ SSymbol "unquote"; x ] ->
            if n = 0 then
                x |> eval envs cont
            else
                x |> replace (n - 1) SUnquote
        | SUnquoteSplicing x
        | SList [ SSymbol "unquote-splicing"; x ] ->
            if n = 0 then
                x |> eval envs cont
            else
                x |> replace (n - 1) SUnquoteSplicing
        | SQuasiquote x
        | SList [ SSymbol "quasiquote"; x ] -> x |> replace (n + 1) SQuasiquote
        | SQuote x
        | SList [ SSymbol "quote"; x ] -> x |> replace n SQuote
        | x -> x

    replace 0 id

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
