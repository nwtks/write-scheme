namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Core =
    let isPromise envs cont =
        function
        | [ SPromise _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    [<TailCall>]
    let rec sForce envs cont =
        function
        | [ SPromise r ] ->
            match r.Value with
            | true, value -> value |> cont
            | false, thunk ->
                thunk
                |> Eval.apply
                    envs
                    (fun result ->
                        match result with
                        | SPromise r2 ->
                            r.Value <- r2.Value
                            sForce envs cont [ SPromise r ]
                        | value ->
                            r.Value <- (true, value)
                            value |> cont)
                    []
        | [ x ] -> x |> cont
        | x -> x |> invalidParameter "'%s' invalid force parameter."

    let sMakePromise envs cont =
        function
        | [ SPromise _ as p ] -> p |> cont
        | [ x ] -> SPromise(ref (true, x)) |> cont
        | x -> x |> invalidParameter "'%s' invalid make-promise parameter."

    let sMakeParameter envs cont =
        function
        | [ init ] -> SParameter(ref init, None) |> cont
        | [ init; converter ] ->
            converter
            |> Eval.apply envs (fun converted -> SParameter(ref converted, Some converter) |> cont) [ init ]
        | x -> x |> invalidParameter "'%s' invalid make-parameter parameter."

    let isEqv envs cont =
        function
        | [ a; b ] -> (a, b) |> eqv |> toSBool |> cont
        | _ -> SFalse |> cont

    let isEqual envs cont =
        function
        | [ a; b ] -> (a, b) |> equal |> toSBool |> cont
        | _ -> SFalse |> cont

    let sNot envs cont =
        function
        | [ SBool false ] -> STrue |> cont
        | _ -> SFalse |> cont

    let isBoolean envs cont =
        function
        | [ SBool _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    let isSymbol envs cont =
        function
        | [ SSymbol _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    let isChar envs cont =
        function
        | [ SChar _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    let isString envs cont =
        function
        | [ SString _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    let isProcedure envs cont =
        function
        | [ SSyntax _ ]
        | [ SProcedure _ ]
        | [ SContinuation _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    [<TailCall>]
    let rec foldApply =
        function
        | acc, []
        | acc, [ SEmpty ] -> List.rev acc
        | acc, [ SList x ] -> List.rev acc @ x
        | acc, [ SPair(x1, x2) ] -> [ SPair(List.rev acc @ x1, x2) ]
        | acc, x1 :: x2 -> (x1 :: acc, x2) |> foldApply

    let sApply envs cont =
        function
        | proc :: args -> proc |> Eval.apply envs cont (([], args) |> foldApply)
        | x -> x |> invalidParameter "'%s' invalid apply parameter."

    [<TailCall>]
    let rec foldTranspose acc =
        function
        | 0, _
        | _, [] -> List.rev acc
        | n, xs -> foldTranspose ((xs |> List.map (List.head >> SQuote)) :: acc) (n - 1, xs |> List.map List.tail)

    let transposeList lists =
        foldTranspose [] (lists |> List.map List.length |> Seq.min, lists)

    [<TailCall>]
    let rec mapMap envs cont proc acc =
        function
        | [] -> List.rev acc |> toSList |> cont
        | x :: xs -> proc |> Eval.apply envs (fun a -> mapMap envs cont proc (a :: acc) xs) x

    let sMap envs cont =
        function
        | [ _ ] as x -> x |> invalidParameter "'%s' invalid map parameter."
        | proc :: lists as x ->
            lists
            |> List.map (function
                | SEmpty -> []
                | SList xs -> xs
                | _ -> x |> invalidParameter "'%s' invalid map parameter.")
            |> transposeList
            |> mapMap envs cont proc []
        | x -> x |> invalidParameter "'%s' invalid map parameter."

    [<TailCall>]
    let rec loopForEach envs cont proc =
        function
        | [] -> SEmpty |> cont
        | x :: xs -> proc |> Eval.apply envs (fun _ -> loopForEach envs cont proc xs) x

    let sForEach envs cont =
        function
        | [ _ ] as x -> x |> invalidParameter "'%s' invalid for-each parameter."
        | proc :: lists as x ->
            lists
            |> List.map (function
                | SEmpty -> []
                | SList xs -> xs
                | _ -> x |> invalidParameter "'%s' invalid for-each parameter.")
            |> transposeList
            |> loopForEach envs cont proc
        | x -> x |> invalidParameter "'%s' invalid for-each parameter."

    let sCallCC envs cont =
        function
        | [ proc ] ->
            let capturedWinders = currentWinders.Value

            let wrappedCont arg =
                let sourceWinders = currentWinders.Value

                if sourceWinders = capturedWinders then
                    cont arg
                else
                    doWind envs sourceWinders capturedWinders (fun _ -> cont arg)

            proc |> Eval.apply envs cont [ SContinuation wrappedCont ]
        | x -> x |> invalidParameter "'%s' invalid call/cc parameter."

    let sValues envs cont =
        function
        | [ x ] -> x |> cont
        | xs -> SValues xs |> cont

    let sCallWithValues envs cont =
        function
        | [ producer; consumer ] ->
            producer
            |> Eval.apply
                envs
                (function
                | SValues xs -> consumer |> Eval.apply envs cont xs
                | x -> consumer |> Eval.apply envs cont [ x ])
                []
        | x -> x |> invalidParameter "'%s' invalid call-with-values parameter."

    let sDynamicWind envs cont =
        function
        | [ inProc; bodyProc; outProc ] ->
            let id = nextWinderId.Value
            nextWinderId.Value <- id + 1

            inProc
            |> Eval.apply
                envs
                (fun _ ->
                    let winder =
                        { Id = id
                          Before = inProc
                          After = outProc }

                    currentWinders.Value <- winder :: currentWinders.Value

                    bodyProc
                    |> Eval.apply
                        envs
                        (fun res ->
                            let nextCur =
                                match currentWinders.Value with
                                | h :: t when h.Id = id -> t
                                | _ -> currentWinders.Value

                            currentWinders.Value <- nextCur
                            outProc |> Eval.apply envs (fun _ -> cont res) [])
                        [])
                []
        | x -> x |> invalidParameter "'%s' invalid dynamic-wind parameter."

    let sWithExceptionHandler envs cont =
        function
        | [ handlerProc; thunkProc ] ->
            let savedWinders = currentWinders.Value

            try
                thunkProc |> Eval.apply envs cont []
            with SchemeRaise obj ->
                handlerProc
                |> Eval.apply
                    envs
                    (fun res -> doWind envs currentWinders.Value savedWinders (fun _ -> cont res))
                    [ SQuote obj ]
        | x -> x |> invalidParameter "'%s' invalid with-exception-handler parameter."

    let sRaise envs cont =
        function
        | [ obj ] -> raise (SchemeRaise obj)
        | x -> x |> invalidParameter "'%s' invalid raise parameter."

    let sError envs cont =
        function
        | SString msg :: irritants -> raise (SchemeRaise(SError(msg, irritants)))
        | x -> x |> invalidParameter "'%s' invalid error parameter."

    let isErrorObject envs cont =
        function
        | [ SError _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    let sErrorObjectMessage envs cont =
        function
        | [ SError(msg, _) ] -> SString msg |> cont
        | x -> x |> invalidParameter "'%s' invalid error-object-message parameter."

    let sErrorObjectIrritants envs cont =
        function
        | [ SError(_, irritants) ] -> irritants |> toSList |> cont
        | x -> x |> invalidParameter "'%s' invalid error-object-irritants parameter."

    let sDisplay envs cont =
        function
        | [ SString x ] ->
            x |> printf "%s"
            SEmpty |> cont
        | [ SChar x ] ->
            x |> printf "%s"
            SEmpty |> cont
        | [ x ] ->
            x |> Print.print |> printf "%s"
            SEmpty |> cont
        | x -> x |> invalidParameter "'%s' invalid display parameter."

    let sLoad envs cont =
        function
        | [ SString f ] ->
            System.IO.File.ReadAllText(f) |> Read.read |> Eval.eval envs cont |> ignore

            sprintf "Loaded '%s'." f |> SSymbol |> cont
        | x -> x |> invalidParameter "'%s' invalid load parameter."
