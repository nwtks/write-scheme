module Eval

open Type
open Read

let extendEnvs envs bindings = (Map.ofList bindings |> Env) :: envs

let lookupEnvs envs symbol =
    match
        List.tryPick
            (fun (env: Env) ->
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

let isEqv eval envs a b =
    let rec eqv =
        function
        | SBool a, SBool b -> a = b
        | SRational (a1, a2), SRational (b1, b2) -> a1 = b1 && a2 = b2
        | SReal a, SReal b -> a = b
        | SString a, SString b -> a = b
        | SChar a, SChar b -> a = b
        | SSymbol a, SSymbol b -> a = b
        | SQuote a, SQuote b -> (a, b) |> eqv
        | SUnquote a, SUnquote b -> (a, b) |> eqv
        | a, b -> a = b

    a
    |> eval envs (fun a' ->
        b
        |> eval envs (fun b' -> (a', b') |> eqv |> newSBool))

let sBegin eval envs cont =
    let rec each res =
        function
        | [] -> res |> cont
        | x :: xs -> x |> eval envs (fun a -> each a xs)

    each SEmpty

let sDefine eval (envs: Env list) cont var expr =
    envs.Head.TryAdd(var, ref SEmpty) |> ignore

    expr
    |> eval envs (fun x ->
        envs.Head.[var].Value <- x
        SEmpty |> cont)

let sSet eval envs cont var expr =
    expr
    |> eval envs (fun x ->
        (lookupEnvs envs var).Value <- x
        SEmpty |> cont)

let zipArgs args =
    let zip xs args' =
        List.zip xs args'
        |> List.map (function
            | (SSymbol p, a) -> p, a)

    function
    | SSymbol x -> [ (x, args |> SList |> SQuote) ]
    | SList xs -> zip xs args
    | SPair (xs, SSymbol rest) ->
        zip xs (args |> List.take (List.length xs))
        @ [ (rest,
             args
             |> List.skip (List.length xs)
             |> SList
             |> SQuote) ]

let sLambda eval envs cont parameters body =
    let rec bind acc envs' cont' =
        function
        | [] ->
            body
            |> eval (List.rev acc |> extendEnvs (envs @ envs')) cont'
        | (p, a) :: xs ->
            a
            |> eval envs' (fun a' -> bind ((p, ref a') :: acc) envs' cont' xs)

    let closure envs' cont' args =
        zipArgs args parameters |> bind [] envs' cont'

    SClosure closure |> cont

let sDefMacro eval envs cont parameters body =
    let closure envs' cont' args =
        body
        |> eval
            (zipArgs args parameters
             |> List.map (fun (p, a) -> p, ref a)
             |> extendEnvs envs)
            (eval envs' cont')

    SClosure closure |> cont

let sIf eval envs cont test conseq alter =
    test
    |> eval envs (function
        | SBool false -> eval envs cont alter
        | _ -> eval envs cont conseq)

let rec sCond eval envs cont =
    let loop test xs conseq =
        test
        |> eval envs (function
            | SBool false -> sCond eval envs cont xs
            | res -> conseq res)

    let rec each res =
        function
        | [] -> res |> cont
        | [ e ] -> eval envs cont e
        | e :: exs -> eval envs (fun _ -> each res exs) e

    function
    | [] -> SEmpty |> cont
    | [ SList (SSymbol "else" :: exs) ] -> each SEmpty exs
    | SList [ test; SSymbol "=>"; op ] :: xs -> loop test xs (fun res -> SList [ op; SQuote res ] |> eval envs cont)
    | SList (test :: exs) :: xs -> loop test xs (fun res -> each res exs)

let rec sAnd eval envs =
    function
    | [] -> STrue
    | [ t ] ->
        t
        |> eval envs (function
            | SBool false -> SFalse
            | x -> x)
    | t :: ts ->
        t
        |> eval envs (function
            | SBool false -> SFalse
            | _ -> sAnd eval envs ts)

let rec sOr eval envs =
    function
    | [] -> SFalse
    | t :: ts ->
        t
        |> eval envs (function
            | SBool false -> sOr eval envs ts
            | x -> x)

let sLet eval envs cont bindings body =
    let rec bind acc body =
        function
        | [] -> eval (List.rev acc |> extendEnvs envs) cont body
        | SList [ SSymbol s; e ] :: xs -> eval envs (fun x -> bind ((s, ref x) :: acc) body xs) e

    bind [] body bindings

let sLetStar eval envs cont bindings body =
    let rec bind envs' body =
        function
        | [] -> eval envs' cont body
        | SList [ SSymbol s; e ] :: xs -> eval envs' (fun x -> bind ([ s, ref x ] |> extendEnvs envs') body xs) e

    bind envs body bindings

let sLetRec eval envs cont bindings body =
    let envs' =
        bindings
        |> List.map (function
            | SList [ SSymbol s; _ ] -> (s, ref SEmpty))
        |> extendEnvs envs

    let rec update =
        function
        | [] -> eval envs' cont body
        | SList [ SSymbol s; e ] :: xs ->
            eval
                envs'
                (fun x ->
                    envs'.Head.[s] := x
                    update xs)
                e

    update bindings

let sLetRecStar eval envs cont bindings body =
    let envs', refs =
        bindings
        |> List.fold
            (fun (envs'', refs') ->
                function
                | SList [ SSymbol s; _ ] ->
                    let r = ref SEmpty
                    [ s, r ] |> extendEnvs envs'', r :: refs')
            (envs, [])

    let rec update =
        function
        | ([], []) -> eval envs' cont body
        | (SList [ _; e ] :: xs, r :: rs) ->
            eval
                envs'
                (fun x ->
                    r := x
                    update (xs, rs))
                e

    update (bindings, List.rev refs)


let sLoad eval envs cont file =
    System.IO.File.ReadAllText(file)
    |> read
    |> eval envs id
    |> ignore

    SSymbol(sprintf "Loaded '%s'." file) |> cont

let apply eval envs cont fn args =
    let rec map acc =
        function
        | [] -> List.rev acc |> fn cont
        | x :: xs -> x |> eval envs (fun a -> xs |> map (a :: acc))

    args |> map []

let rec eval envs cont =
    function
    | SEmpty
    | SBool _
    | SRational _
    | SReal _
    | SString _
    | SChar _
    | SClosure _
    | FFunction _ as e -> e |> cont
    | SSymbol s -> (lookupEnvs envs s).Value |> cont
    | SQuote datum -> sQuote eval envs cont datum
    | SList [] -> SEmpty |> cont
    | SList [ SSymbol "quote"; datum ] -> SQuote datum |> eval envs cont
    | SList [ SSymbol "quasiquote"; datum ] -> SQuasiquote datum |> eval envs cont
    | SList [ SSymbol "unquote"; datum ] -> SUnquote datum |> eval envs cont
    | SList [ SSymbol "unquote-splicing"; datum ] -> SUnquoteSplicing datum |> eval envs cont
    | SList [ SSymbol "eqv?"; a; b ]
    | SList [ SSymbol "eq?"; a; b ] -> isEqv eval envs a b |> cont
    | SList (SSymbol "begin" :: bodys) -> sBegin eval envs cont bodys
    | SList [ SSymbol "define"; SSymbol var; expr ] -> sDefine eval envs cont var expr
    | SList [ SSymbol "set!"; SSymbol var; expr ] -> sSet eval envs cont var expr
    | SList [ SSymbol "lambda"; parameters; body ] -> sLambda eval envs cont parameters body
    | SList [ SSymbol "define"; SList [ SSymbol var; formals ]; body ] ->
        sLambda eval envs cont formals body
        |> sDefine eval envs cont var
    | SList [ SSymbol "define"; SPair ([ SSymbol var ], formal); body ] ->
        sLambda eval envs cont formal body
        |> sDefine eval envs cont var
    | SList [ SSymbol "defmacro"; parameters; body ] -> sDefMacro eval envs cont parameters body
    | SList [ SSymbol "if"; test; conseq; alter ] -> sIf eval envs cont test conseq alter
    | SList [ SSymbol "if"; test; conseq ] -> sIf eval envs cont test conseq SEmpty
    | SList (SSymbol "cond" :: clauses) -> sCond eval envs cont clauses
    | SList (SSymbol "and" :: tests) -> sAnd eval envs tests |> cont
    | SList (SSymbol "or" :: tests) -> sOr eval envs tests |> cont
    | SList [ SSymbol "let"; SList bindings; body ] -> sLet eval envs cont bindings body
    | SList [ SSymbol "let*"; SList bindings; body ] -> sLetStar eval envs cont bindings body
    | SList [ SSymbol "letrec"; SList bindings; body ] -> sLetRec eval envs cont bindings body
    | SList [ SSymbol "letrec*"; SList bindings; body ] -> sLetRecStar eval envs cont bindings body
    | SList [ SSymbol "load"; SString file ] -> sLoad eval envs cont file
    | SList (operator :: operand) ->
        operator
        |> eval envs (function
            | SClosure fn -> fn envs cont operand
            | FFunction fn -> apply eval envs cont fn operand)
