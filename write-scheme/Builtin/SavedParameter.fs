namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module SavedParameter =
    type SavedParameter =
        { Ref: SExpression ref
          SavedValue: SExpression ref }

    let eachParamBinding =
        function
        | SPair { car = param
                  cdr = SPair { car = expr; cdr = SEmpty, _ }, _ },
          _ -> param, expr
        | x -> x |> invalid (snd x) "'%s' invalid parameterize binding."

    [<TailCall>]
    let rec bindParameterize envs pos cont body saved =
        function
        | [] ->
            let id = Context.getNextWinderId envs
            let savedRev = List.rev saved

            let swapThunk =
                SProcedure(fun _ pos' cont' _ ->
                    savedRev
                    |> List.iter (fun s ->
                        let tmp = s.Ref.Value
                        s.Ref.Value <- s.SavedValue.Value
                        s.SavedValue.Value <- tmp)

                    (SUnspecified, pos') |> cont'),
                pos

            let winder =
                { id = id
                  before = swapThunk
                  after = swapThunk }

            Context.pushWinder envs winder

            body
            |> Eval.eachEval
                envs
                (fun res ->
                    Context.popWinder envs id
                    swapThunk |> Eval.apply envs (fun _ -> cont res) [])
                (SEmpty, pos)
        | (param, expr) :: xs ->
            param
            |> Eval.eval envs (function
                | SParameter(r, converterOpt), _ ->
                    expr
                    |> Eval.eval envs (fun v ->
                        match converterOpt with
                        | Some converter ->
                            converter
                            |> Eval.apply
                                envs
                                (fun converted ->
                                    let old = r.Value
                                    r.Value <- converted
                                    let s = { Ref = r; SavedValue = ref old }
                                    bindParameterize envs (snd converter) cont body (s :: saved) xs)
                                [ v ]
                        | None ->
                            let old = r.Value
                            r.Value <- v
                            let s = { Ref = r; SavedValue = ref old }
                            bindParameterize envs pos cont body (s :: saved) xs)
                | _ -> param |> invalid (snd param) "'%s' is not a parameter in parameterize.")

    let sMakeParameter envs pos cont =
        function
        | [ init ] -> (SParameter(ref init, None), pos) |> cont
        | [ init; converter ] ->
            converter
            |> Eval.apply envs (fun converted -> (SParameter(ref converted, Some converter), pos) |> cont) [ init ]
        | x -> x |> invalidParameter pos "'%s' invalid make-parameter parameter."
