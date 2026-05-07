namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Helper =
    let invalid pos fmt =
        Print.print
        >> fun s ->
            let msg = sprintf (Printf.StringFormat<string -> string> fmt) s
            EvalError(msg, pos) |> Error

    let invalidParameter pos fmt = toSPair >> invalid pos fmt

    [<TailCall>]
    let rec loopMapResult f acc =
        function
        | [] -> acc |> List.rev |> Ok
        | x :: xs ->
            match f x with
            | Ok r -> xs |> loopMapResult f (r :: acc)
            | Error e -> Error e

    let mapResult f = loopMapResult f []

    let eachBinding =
        function
        | SPair { car = SSymbol variable, _
                  cdr = SPair { car = expression; cdr = SEmpty, _ }, _ },
          _ -> Ok(variable, expression)
        | x -> x |> invalid (snd x) "'%s' invalid binding."

    [<TailCall>]
    let rec eqv (a, b) =
        match a, b with
        | (SBool x, _), (SBool y, _) -> x = y
        | (SSymbol x, _), (SSymbol y, _) -> x = y
        | (SRational(n1, d1), _), (SRational(n2, d2), _) -> n1 = n2 && d1 = d2
        | (SReal x, _), (SReal y, _) -> x = y
        | (SComplex x, _), (SComplex y, _) -> x = y
        | (SChar x, _), (SChar y, _) -> x = y
        | (SEmpty, _), (SEmpty, _) -> true
        | (SPair x, _), (SPair y, _) -> LanguagePrimitives.PhysicalEquality x y
        | (SVector x, _), (SVector y, _) -> LanguagePrimitives.PhysicalEquality x y
        | (SByteVector x, _), (SByteVector y, _) -> LanguagePrimitives.PhysicalEquality x y
        | (SContinuation x, _), (SContinuation y, _) -> LanguagePrimitives.PhysicalEquality x y
        | (SProcedure x, _), (SProcedure y, _) -> LanguagePrimitives.PhysicalEquality x y
        | (SQuote x, _), (SQuote y, _) -> eqv (x, y)
        | (SQuasiquote x, _), (SQuasiquote y, _) -> eqv (x, y)
        | (SUnquote x, _), (SUnquote y, _) -> eqv (x, y)
        | (SUnquoteSplicing x, _), (SUnquoteSplicing y, _) -> eqv (x, y)
        | (x, _), (y, _) -> LanguagePrimitives.PhysicalEquality x y

    [<TailCall>]
    let rec loopDiffWinders sList tList lenS lenT accS accT =
        if lenS > lenT then
            match sList with
            | hd :: tl -> loopDiffWinders tl tList (lenS - 1) lenT (hd :: accS) accT
            | [] -> failwith "unreachable."
        elif lenT > lenS then
            match tList with
            | hd :: tl -> loopDiffWinders sList tl lenS (lenT - 1) accS (hd :: accT)
            | [] -> failwith "unreachable."
        else
            match sList, tList with
            | h1 :: _, h2 :: _ when h1.id = h2.id -> List.rev accS, List.rev accT
            | h1 :: t1, h2 :: t2 -> loopDiffWinders t1 t2 (lenS - 1) (lenT - 1) (h1 :: accS) (h2 :: accT)
            | _ -> List.rev accS, List.rev accT

    [<TailCall>]
    let rec runWindLeaves envs next current =
        function
        | [] -> Ok current |> next
        | head :: rest ->
            let nextCurrent = Context.leaveWinder envs current head.id

            head.after
            |> Eval.apply
                envs
                (function
                | Ok _ -> rest |> runWindLeaves envs next nextCurrent
                | Error e -> Error e |> next)
                []

    [<TailCall>]
    let rec runWindEnters envs next current =
        function
        | [] -> Ok current |> next
        | head :: rest ->
            head.before
            |> Eval.apply
                envs
                (function
                | Ok _ ->
                    let nextCurrent = head |> Context.enterWinder envs current
                    rest |> runWindEnters envs next nextCurrent
                | Error e -> Error e |> next)
                []

    let doWind envs cont savedWinders arg =
        let currentWinders = envs.winders.Value

        let leaves, enters =
            loopDiffWinders currentWinders savedWinders (List.length currentWinders) (List.length savedWinders) [] []

        let entersRev = List.rev enters

        leaves
        |> runWindLeaves
            envs
            (function
            | Ok current -> entersRev |> runWindEnters envs (fun _ -> cont arg) current
            | Error e -> Error e |> cont)
            currentWinders

    let doAroundProc envs cont before thunk after =
        let id = Context.getNextWinderId envs

        before
        |> Eval.apply
            envs
            (function
            | Ok _ ->
                let winder =
                    { id = id
                      before = before
                      after = after }

                winder |> Context.pushWinder envs

                thunk
                |> Eval.apply
                    envs
                    (fun res ->
                        Context.popWinder envs id
                        after |> Eval.apply envs (fun _ -> cont res) [])
                    []
            | x -> x |> cont)
            []
