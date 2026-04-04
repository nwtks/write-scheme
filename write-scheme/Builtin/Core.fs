namespace WriteScheme.Builtins

open WriteScheme
open Type
open Eval
open Read
open Print

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

    let sApply envs cont =
        let rec fold =
            function
            | acc, []
            | acc, [ SEmpty ] -> List.rev acc
            | acc, [ SList x ] -> List.rev acc @ x
            | acc, [ SPair(x1, x2) ] -> [ SPair(List.rev acc @ x1, x2) ]
            | acc, x1 :: x2 -> (x1 :: acc, x2) |> fold

        function
        | proc :: args -> apply envs cont (([], args) |> fold) proc
        | x -> x |> invalidParameter "'%s' invalid apply parameter."

    let transposeList lists =
        let rec fold acc =
            function
            | 0, _
            | _, [] -> List.rev acc
            | n, xs -> fold ((xs |> List.map (List.head >> SQuote)) :: acc) (n - 1, xs |> List.map List.tail)

        fold [] (lists |> List.map List.length |> Seq.min, lists)

    let sMap envs cont =
        let rec map proc acc =
            function
            | [] -> List.rev acc |> newList |> cont
            | x :: xs -> apply envs (fun a -> map proc (a :: acc) xs) x proc

        function
        | [ _ ] as x -> x |> invalidParameter "'%s' invalid map parameter."
        | proc :: lists as x ->
            lists
            |> List.map (function
                | SEmpty -> []
                | SList xs -> xs
                | _ -> x |> invalidParameter "'%s' invalid map parameter.")
            |> transposeList
            |> map proc []
        | x -> x |> invalidParameter "'%s' invalid map parameter."

    let sForEach envs cont =
        let rec loop proc =
            function
            | [] -> SEmpty |> cont
            | x :: xs -> apply envs (fun _ -> loop proc xs) x proc

        function
        | [ _ ] as x -> x |> invalidParameter "'%s' invalid for-each parameter."
        | proc :: lists as x ->
            lists
            |> List.map (function
                | SEmpty -> []
                | SList xs -> xs
                | _ -> x |> invalidParameter "'%s' invalid for-each parameter.")
            |> transposeList
            |> loop proc
        | x -> x |> invalidParameter "'%s' invalid for-each parameter."

    let sCallCC envs cont =
        function
        | [ proc ] -> apply envs cont [ SContinuation cont ] proc
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
            x |> print |> printf "%s"
            SEmpty |> cont
        | x -> x |> invalidParameter "'%s' invalid display parameter."

    let sLoad envs cont =
        function
        | [ SString f ] ->
            System.IO.File.ReadAllText(f) |> read |> eval envs cont |> ignore

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
                apply envs cont [] thunkProc
            with SchemeRaise obj ->
                apply envs cont [ obj ] handlerProc
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
        | [ SError(_, irritants) ] -> irritants |> newList |> cont
        | x -> x |> invalidParameter "'%s' invalid error-object-irritants parameter."
