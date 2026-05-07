namespace WriteScheme

open Type

module Context =
    let initialHandlers =
        [ SProcedure(fun _ pos cont ->
              function
              | [ obj ] -> Error(SchemeRaise(obj, pos)) |> cont
              | _ -> failwith "unreachable."),
          None ]

    let empty =
        { environments = []
          nextExpansionId = 0
          nextRecordTypeId = 0
          winders = ref []
          nextWinderId = ref 0
          handlers = ref initialHandlers }

    let reset envs =
        envs.winders.Value <- []
        envs.handlers.Value <- initialHandlers

    let extendEnvs envs bindings =
        { envs with
            environments = (Map.ofList bindings |> ref) :: envs.environments }

    let mergeEnvs envs captureEnvs =
        { envs with
            environments = envs.environments @ captureEnvs.environments }

    let tryLookupEnv (env: Environment) symbol = env.Value |> Map.tryFind symbol

    let defineEnvVar envs symbol value =
        let env = envs.environments.Head

        symbol
        |> tryLookupEnv env
        |> function
            | Some r -> r.Value <- value
            | None -> env.Value <- env.Value |> Map.add symbol (ref value)

    let tryLookupEnvs envs symbol =
        envs.environments |> List.tryPick (fun env -> tryLookupEnv env symbol)

    let lookupEnvs envs pos symbol =
        symbol
        |> tryLookupEnvs envs
        |> function
            | Some x -> Ok x
            | None -> EvalError(sprintf "No binding for '%s'." symbol, pos) |> Error

    let getNextRecordTypeId envs =
        envs.nextRecordTypeId <- envs.nextRecordTypeId + 1
        envs.nextRecordTypeId

    let getNextExpansionId envs =
        envs.nextExpansionId <- envs.nextExpansionId + 1
        envs.nextExpansionId

    let enterWinder envs current winder =
        let next = winder :: current
        envs.winders.Value <- next
        next

    let leaveWinder envs current id =
        let next =
            match current with
            | h :: t when h.id = id -> t
            | x -> x

        envs.winders.Value <- next
        next

    let pushWinder envs winder =
        winder |> enterWinder envs envs.winders.Value |> ignore

    let popWinder envs id =
        leaveWinder envs envs.winders.Value id |> ignore

    let getNextWinderId envs =
        envs.nextWinderId.Value <- envs.nextWinderId.Value + 1
        envs.nextWinderId.Value

    let pushHandler envs handler =
        envs.handlers.Value <- handler :: envs.handlers.Value

    let popHandler envs =
        let handler = envs.handlers.Value.Head
        envs.handlers.Value <- envs.handlers.Value.Tail
        handler
