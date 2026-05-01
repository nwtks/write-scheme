namespace WriteScheme

open Type

module Context =
    let empty =
        { environments = []
          nextExpansionId = 0
          nextRecordTypeId = 0
          currentWinders = ref []
          nextWinderId = ref 0 }

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
        | Some x -> x
        | None -> failwithf "No binding for '%s'.%s" symbol (pos |> formatPosition)

    let getNextRecordTypeId envs =
        envs.nextRecordTypeId <- envs.nextRecordTypeId + 1
        envs.nextRecordTypeId

    let getNextExpansionId envs =
        envs.nextExpansionId <- envs.nextExpansionId + 1
        envs.nextExpansionId

    let setWinders envs winders = envs.currentWinders.Value <- winders

    let enterWinder envs cur winder =
        let next = winder :: cur
        setWinders envs next
        next

    let leaveWinder envs cur id =
        let next =
            match cur with
            | h :: t when h.id = id -> t
            | xs -> xs

        setWinders envs next
        next

    let pushWinder envs winder =
        enterWinder envs envs.currentWinders.Value winder |> ignore

    let popWinder envs id =
        leaveWinder envs envs.currentWinders.Value id |> ignore

    let getNextWinderId envs =
        envs.nextWinderId.Value <- envs.nextWinderId.Value + 1
        envs.nextWinderId.Value
