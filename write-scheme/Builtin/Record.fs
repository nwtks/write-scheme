namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Record =
    let parseRecordFields specs =
        specs
        |> mapResult (function
            | SPair { car = SSymbol fName, _
                      cdr = SPair { car = SSymbol aName, _; cdr = rest }, _ },
              _ ->
                let mName =
                    match rest with
                    | SPair { car = SSymbol m, _; cdr = SEmpty, _ }, _ -> Some m
                    | _ -> None

                Ok(fName, aName, mName)
            | x -> x |> invalid (snd x) "'%s' invalid record field spec.")

    let recordConstructorProc
        typeId
        name
        constructorName
        (constructorFields: SExpression list)
        (fieldNames: string list)
        context
        pos
        cont
        (args: SExpression list)
        =
        if args.Length <> constructorFields.Length then
            EvalError(
                sprintf "%s requires %d arguments, but got %d." constructorName constructorFields.Length args.Length,
                pos
            )
            |> Error
            |> cont
        else
            let recordFields = Array.init fieldNames.Length (fun _ -> ref (SUnspecified, pos))
            let mutable error = None

            args
            |> List.zip constructorFields
            |> List.iter (fun (field, value) ->
                if error.IsNone then
                    match field with
                    | SSymbol fieldName, _ ->
                        let idx = fieldNames |> List.findIndex ((=) fieldName)
                        recordFields.[idx].Value <- value
                    | _ ->
                        error <-
                            EvalError("Constructor field mapping failed: not a symbol", pos)
                            |> Error
                            |> Some)

            error
            |> Option.defaultWith (fun () -> Ok(SRecord(typeId, name, recordFields), pos))
            |> cont

    let recordPredProc typeId context pos cont =
        function
        | [ SRecord(tid, _, _), _ ] -> Ok(tid = typeId |> toSBool, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let recordFieldAccessorProc typeId name idx accessorName context pos cont =
        function
        | [ SRecord(tid, _, fs), _ ] when tid = typeId -> Ok fs.[idx].Value |> cont
        | [ x ] ->
            EvalError(sprintf "Accessor %s expected %s, but got %s." accessorName name (x |> Print.print), x |> snd)
            |> Error
            |> cont
        | _ ->
            EvalError(sprintf "Accessor %s requires 1 argument." accessorName, pos)
            |> Error
            |> cont

    let recordFieldModifierProc typeId name idx modifierName context pos cont =
        function
        | [ SRecord(tid, _, fs), _; v ] when tid = typeId ->
            fs.[idx].Value <- v
            Ok(SUnspecified, pos) |> cont
        | [ x; _ ] ->
            EvalError(sprintf "Modifier %s expected %s, but got %s." modifierName name (x |> Print.print), x |> snd)
            |> Error
            |> cont
        | _ ->
            EvalError(sprintf "Modifier %s requires 2 arguments." modifierName, pos)
            |> Error
            |> cont

    let sDefineRecordType context pos cont =
        function
        | (SSymbol name, _) :: (SPair { car = SSymbol constructorName, _
                                        cdr = constructorFields },
                                _) :: (SSymbol pred, _) :: fields ->
            let defineProc name proc =
                Context.defineEnvironmentVariable context name (proc |> SProcedure, pos)

            let typeId = Context.getNextRecordTypeId context

            match constructorFields |> toList with
            | Ok ctorFields ->
                match parseRecordFields fields with
                | Ok fieldSpecs ->
                    let fieldNames = fieldSpecs |> List.map (fun (n, _, _) -> n)
                    defineProc constructorName (recordConstructorProc typeId name constructorName ctorFields fieldNames)
                    defineProc pred (recordPredProc typeId)

                    fieldSpecs
                    |> List.iteri (fun idx (_, accessorName, modifierNameOpt) ->
                        defineProc accessorName (recordFieldAccessorProc typeId name idx accessorName)

                        modifierNameOpt
                        |> Option.iter (fun modifierName ->
                            defineProc modifierName (recordFieldModifierProc typeId name idx modifierName)))

                    Ok(SUnspecified, pos) |> cont
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid define-record-type parameter." |> cont
