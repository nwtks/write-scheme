namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Core =
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
        | proc :: args -> Eval.apply envs cont (([], args) |> foldApply) proc
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
        | x :: xs -> Eval.apply envs (fun a -> mapMap envs cont proc (a :: acc) xs) x proc

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
        | x :: xs -> Eval.apply envs (fun _ -> loopForEach envs cont proc xs) x proc

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
        | [ proc ] -> Eval.apply envs cont [ SContinuation cont ] proc
        | x -> x |> invalidParameter "'%s' invalid call/cc parameter."

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

    let sRaise envs cont =
        function
        | [ obj ] -> raise (SchemeRaise obj)
        | x -> x |> invalidParameter "'%s' invalid raise parameter."

    let sWithExceptionHandler envs cont =
        function
        | [ handlerProc; thunkProc ] ->
            try
                Eval.apply envs cont [] thunkProc
            with SchemeRaise obj ->
                Eval.apply envs cont [ obj ] handlerProc
        | x -> x |> invalidParameter "'%s' invalid with-exception-handler parameter."

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
