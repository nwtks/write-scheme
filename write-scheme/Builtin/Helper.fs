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
    let rec eqv =
        function
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
            | h1 :: _, h2 :: _ when h1.id = h2.id -> accS |> List.rev, accT |> List.rev
            | h1 :: t1, h2 :: t2 -> loopDiffWinders t1 t2 (lenS - 1) (lenT - 1) (h1 :: accS) (h2 :: accT)
            | _ -> accS |> List.rev, accT |> List.rev

    [<TailCall>]
    let rec runWindLeaves context next current =
        function
        | [] -> Ok current |> next
        | head :: rest ->
            let nextCurrent = Context.leaveWinder context current head.id

            head.after
            |> Eval.apply
                context
                (function
                | Ok _ -> rest |> runWindLeaves context next nextCurrent
                | Error e -> Error e |> next)
                []

    [<TailCall>]
    let rec runWindEnters context next current =
        function
        | [] -> Ok current |> next
        | head :: rest ->
            head.before
            |> Eval.apply
                context
                (function
                | Ok _ ->
                    let nextCurrent = head |> Context.enterWinder context current
                    rest |> runWindEnters context next nextCurrent
                | Error e -> Error e |> next)
                []

    let doWind context cont savedWinders arg =
        let currentWinders = context.winders.Value

        let leaves, enters =
            loopDiffWinders
                currentWinders
                savedWinders
                (currentWinders |> List.length)
                (savedWinders |> List.length)
                []
                []

        let entersRev = enters |> List.rev

        leaves
        |> runWindLeaves
            context
            (function
            | Ok current -> entersRev |> runWindEnters context (fun _ -> cont arg) current
            | Error e -> Error e |> cont)
            currentWinders

    let doAroundProc context cont before thunk after =
        let id = Context.getNextWinderId context

        before
        |> Eval.apply
            context
            (function
            | Ok _ ->
                let winder =
                    { id = id
                      before = before
                      after = after }

                winder |> Context.pushWinder context

                thunk
                |> Eval.apply
                    context
                    (fun res ->
                        Context.popWinder context id
                        after |> Eval.apply context (fun _ -> cont res) [])
                    []
            | x -> x |> cont)
            []

    let tryReadAll foldCase filename pos =
        let path = filename.runes |> runesToString

        try
            path |> System.IO.File.ReadAllText |> Read.readAll foldCase
        with
        | :? System.IO.FileNotFoundException -> EvalError(sprintf "File not found: %s." path, pos) |> Error
        | ex -> EvalError(sprintf "Error reading file %s: %s." path ex.Message, pos) |> Error

    let getRange (length: int) =
        function
        | [] -> Some(0, length)
        | [ SRational(start, d), _ ] when d = 1I && start >= 0I && start <= bigint length -> Some(int start, length)
        | [ SRational(start, d1), _; SRational(stop, d2), _ ] when
            d1 = 1I && d2 = 1I && start >= 0I && stop >= start && stop <= bigint length
            ->
            Some(int start, int stop)
        | _ -> None
