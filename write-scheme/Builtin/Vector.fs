namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Vector =
    let isVector envs cont =
        function
        | [ SVector _ ] -> STrue |> cont
        | [ _ ] -> SFalse |> cont
        | x -> x |> invalidParameter "'%s' invalid vector? parameter."

    let sMakeVector envs cont =
        function
        | [ SRational(k, d) ] when d = 1I && k >= 0I -> Array.create (int k) SUnspecified |> SVector |> cont
        | [ SRational(k, d); fill ] when d = 1I && k >= 0I -> Array.create (int k) fill |> SVector |> cont
        | x -> x |> invalidParameter "'%s' invalid make-vector parameter."

    let sVector envs cont xs = xs |> List.toArray |> SVector |> cont

    let sVectorLength envs cont =
        function
        | [ SVector xs ] -> SRational(bigint xs.Length, 1I) |> cont
        | x -> x |> invalidParameter "'%s' invalid vector-length parameter."

    let sVectorRef envs cont =
        function
        | [ SVector xs; SRational(k, d) ] when d = 1I && k >= 0I && k < bigint xs.Length -> xs.[int k] |> cont
        | x -> x |> invalidParameter "'%s' invalid vector-ref parameter."

    let sVectorSet envs cont =
        function
        | [ SVector xs; SRational(k, d); obj ] when d = 1I && k >= 0I && k < bigint xs.Length ->
            xs.[int k] <- obj
            SUnspecified |> cont
        | x -> x |> invalidParameter "'%s' invalid vector-set! parameter."

    let sVectorToList envs cont =
        function
        | [ SVector xs ] -> xs |> Array.toList |> toSList |> cont
        | x -> x |> invalidParameter "'%s' invalid vector->list parameter."

    let sListToVector envs cont =
        function
        | [ SList xs ] -> xs |> List.toArray |> SVector |> cont
        | [ SEmpty ] -> [||] |> SVector |> cont
        | x -> x |> invalidParameter "'%s' invalid list->vector parameter."

    let sVectorFill envs cont =
        function
        | [ SVector xs; fill ] ->
            for i in 0 .. xs.Length - 1 do
                xs.[i] <- fill

            SUnspecified |> cont
        | x -> x |> invalidParameter "'%s' invalid vector-fill! parameter."
