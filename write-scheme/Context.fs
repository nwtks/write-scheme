namespace WriteScheme

open Type

module Context =
    let empty =
        { environments = []
          nextExpansionId = 0
          nextRecordTypeId = 0
          nextWinderId = 0 }

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

    let lookupEnvs envs symbol =
        match tryLookupEnvs envs symbol with
        | Some x -> x
        | None -> sprintf "No binding for '%s'." symbol |> failwith

    let getNextRecordTypeId envs =
        envs.nextRecordTypeId <- envs.nextRecordTypeId + 1
        envs.nextRecordTypeId

    let getNextExpansionId envs =
        envs.nextExpansionId <- envs.nextExpansionId + 1
        envs.nextExpansionId

    let getNextWinderId envs =
        envs.nextWinderId <- envs.nextWinderId + 1
        envs.nextWinderId
