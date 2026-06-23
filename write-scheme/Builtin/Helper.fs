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

    let wrapUnary name fn =
        let fmt = $"'%%s' invalid {name} parameter."

        fun context pos cont ->
            function
            | [ x ] -> fn x |> Result.map (fun v -> v, pos) |> cont
            | x -> x |> invalidParameter pos fmt |> cont

    let wrapUnaryPred name pred =
        wrapUnary name (fun x -> Ok(pred (fst x) |> toSBool))

    let wrapUnarySExprPred name pred =
        wrapUnary name (fun x -> Ok(pred x |> toSBool))

    let eachBinding =
        function
        | SPair { car = SSymbol variable, _
                  cdr = SPair { car = expression; cdr = SEmpty, _ }, _ },
          _ -> Ok(variable, expression)
        | x -> x |> invalid (snd x) "'%s' invalid binding."

    [<TailCall>]
    let rec eqv ((a, _), (b, _)) =
        match a, b with
        | SBool x, SBool y -> x = y
        | SSymbol x, SSymbol y -> x = y
        | SRational(n1, d1), SRational(n2, d2) -> n1 = n2 && d1 = d2
        | SReal x, SReal y -> x = y
        | SComplex x, SComplex y -> x = y
        | SChar x, SChar y -> x = y
        | SEmpty, SEmpty -> true
        | SQuote x, SQuote y
        | SQuasiquote x, SQuasiquote y
        | SUnquote x, SUnquote y
        | SUnquoteSplicing x, SUnquoteSplicing y -> eqv (x, y)
        | _ -> LanguagePrimitives.PhysicalEquality a b

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
        | :? System.IO.FileNotFoundException -> EvalError($"File not found: {path}.", pos) |> Error
        | ex -> EvalError($"Error reading file {path}: {ex.Message}.", pos) |> Error

    let readAndResolveInclude foldCase filename pos =
        tryReadAll foldCase filename pos
        |> Result.bind (fun exprs -> exprs |> mapResult DatumLabel.resolveLabels)

    let getRange (length: int) =
        function
        | [] -> Some(0, length)
        | [ SRational(start, d), _ ] when d = 1I && start >= 0I && start <= bigint length -> Some(int start, length)
        | [ SRational(start, d1), _; SRational(stop, d2), _ ] when
            d1 = 1I && d2 = 1I && start >= 0I && stop >= start && stop <= bigint length
            ->
            Some(int start, int stop)
        | _ -> None
