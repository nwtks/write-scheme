module Type

[<ReferenceEqualityAttribute>]
type SExpression =
    | SEmpty
    | SBool of bool
    | SNumber of float
    | SString of string
    | SChar of string
    | SSymbol of string
    | SList of SExpression list
    | SPair of SExpression list * SExpression
    | SQuote of SExpression
    | SUnquote of SExpression
    | SClosure of (Env list -> Continuation -> SExpression list -> SExpression)
    | FFunction of (Continuation -> SExpression list -> SExpression)

and Env = System.Collections.Generic.Dictionary<string, SExpression ref>

and Continuation = SExpression -> SExpression

let extendEnvs envs bindings = (Map.ofList bindings |> Env) :: envs

let lookupEnvs envs symbol =
    match List.tryPick
              (fun (env: Env) ->
                  match env.TryGetValue symbol with
                  | true, v -> Some v
                  | _ -> None)
              envs with
    | Some v -> v
    | None -> sprintf "No binding for '%s'." symbol |> failwith
