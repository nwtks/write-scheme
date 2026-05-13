namespace WriteScheme

open Type

module Context =
    let initialHandlers =
        [ SProcedure(fun _ pos cont ->
              function
              | [ obj ] -> SchemeRaise(obj, pos) |> Error |> cont
              | _ -> failwith "unreachable."),
          None ]

    let empty =
        { environments = []
          libraries = ref Map.empty
          nextExpansionId = 0
          nextRecordTypeId = 0
          winders = ref []
          nextWinderId = ref 0
          handlers = ref initialHandlers }

    let reset context =
        context.winders.Value <- []
        context.handlers.Value <- initialHandlers

    let extendEnvironments context bindings =
        { context with
            environments = (Map.ofList bindings |> ref) :: context.environments }

    let mergeEnvironments context captureContext =
        { context with
            environments = context.environments @ captureContext.environments }

    let tryLookupEnvironment (environment: Environment) symbol = environment.Value |> Map.tryFind symbol

    let defineEnvironmentVariable context symbol value =
        let env = context.environments.Head

        symbol
        |> tryLookupEnvironment env
        |> function
            | Some r -> r.Value <- value
            | None -> env.Value <- env.Value |> Map.add symbol (ref value)

    let tryLookupEnvironments context symbol =
        context.environments
        |> List.tryPick (fun env -> symbol |> tryLookupEnvironment env)

    let lookupEnvironments context pos symbol =
        symbol
        |> tryLookupEnvironments context
        |> function
            | Some x -> Ok x
            | None -> EvalError(sprintf "No binding for '%s'." symbol, pos) |> Error

    let registerLibrary context name libEnvironment exports =
        let libName = name |> Print.print

        let lib =
            { name = libName
              environment = libEnvironment
              exports = exports }

        context.libraries.Value <- context.libraries.Value |> Map.add libName lib

    let lookupLibrary context pos name =
        let libName = name |> Print.print

        match context.libraries.Value |> Map.tryFind libName with
        | Some lib -> Ok lib
        | None -> EvalError(sprintf "Library '%s' not found." libName, pos) |> Error

    let getNextExpansionId context =
        context.nextExpansionId <- context.nextExpansionId + 1
        context.nextExpansionId

    let getNextRecordTypeId context =
        context.nextRecordTypeId <- context.nextRecordTypeId + 1
        context.nextRecordTypeId

    let enterWinder context current winder =
        let next = winder :: current
        context.winders.Value <- next
        next

    let leaveWinder context current id =
        let next =
            match current with
            | h :: t when h.id = id -> t
            | x -> x

        context.winders.Value <- next
        next

    let pushWinder context winder =
        winder |> enterWinder context context.winders.Value |> ignore

    let popWinder context id =
        leaveWinder context context.winders.Value id |> ignore

    let getNextWinderId context =
        context.nextWinderId.Value <- context.nextWinderId.Value + 1
        context.nextWinderId.Value

    let pushHandler context handler =
        context.handlers.Value <- handler :: context.handlers.Value

    let popHandler context =
        let handler = context.handlers.Value.Head
        context.handlers.Value <- context.handlers.Value.Tail
        handler
