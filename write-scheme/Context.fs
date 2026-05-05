namespace WriteScheme

open Type

module Context =
    let initialHandlers =
        [ SProcedure(fun _ pos cont ->
              function
              | [ obj ] -> Error(SchemeRaise(obj, pos)) |> cont
              | _ -> failwith "unreachable"),
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

    let tryLookupEnv (env: Environment) symbol = Map.tryFind symbol env.Value

    let defineEnvVar envs symbol value =
        let env = envs.environments.Head

        match tryLookupEnv env symbol with
        | Some r -> r.Value <- value
        | None -> env.Value <- Map.add symbol (ref value) env.Value

    let tryLookupEnvs envs symbol =
        List.tryPick (fun env -> tryLookupEnv env symbol) envs.environments

    let lookupEnvs envs pos symbol =
        match tryLookupEnvs envs symbol with
        | Some x -> Ok x
        | None -> Error(EvalError(sprintf "No binding for '%s'." symbol, pos))

    let getNextRecordTypeId envs =
        envs.nextRecordTypeId <- envs.nextRecordTypeId + 1
        envs.nextRecordTypeId

    let getNextExpansionId envs =
        envs.nextExpansionId <- envs.nextExpansionId + 1
        envs.nextExpansionId

    let enterWinder envs cur winder =
        let next = winder :: cur
        envs.winders.Value <- next
        next

    let leaveWinder envs cur id =
        let next =
            match cur with
            | h :: t when h.id = id -> t
            | xs -> xs

        envs.winders.Value <- next
        next

    let pushWinder envs winder =
        enterWinder envs envs.winders.Value winder |> ignore

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
