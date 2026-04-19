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
        | [ SVector xs ] -> newSRational (bigint xs.Length) 1I |> cont
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

    let getVectorRange (length: int) =
        function
        | [ _ ] -> Some(0, length)
        | [ _; SRational(start, d) ] when d = 1I && start >= 0I && start <= bigint length -> Some(int start, length)
        | [ _; SRational(start, d1); SRational(stop, d2) ] when
            d1 = 1I && d2 = 1I && start >= 0I && stop >= start && stop <= bigint length
            ->
            Some(int start, int stop)
        | _ -> None

    let sVectorToList envs cont =
        function
        | SVector xs :: _ as args ->
            match getVectorRange xs.Length args with
            | Some(start, stop) -> xs.[start .. stop - 1] |> Array.toList |> toSList |> cont
            | None -> args |> invalidParameter "'%s' invalid vector->list parameter."
        | x -> x |> invalidParameter "'%s' invalid vector->list parameter."

    let sListToVector envs cont =
        function
        | [ SList xs ] -> xs |> List.toArray |> SVector |> cont
        | [ SEmpty ] -> [||] |> SVector |> cont
        | x -> x |> invalidParameter "'%s' invalid list->vector parameter."

    let sVectorToString envs cont =
        function
        | SVector xs :: _ as args ->
            match getVectorRange xs.Length args with
            | Some(start, stop) ->
                let runes =
                    xs.[start .. stop - 1]
                    |> Array.map (function
                        | SChar c -> c
                        | x -> Print.print x |> sprintf "'%s' is not a char." |> failwith)

                { runes = runes; isImmutable = false } |> SString |> cont
            | None -> args |> invalidParameter "'%s' invalid vector->string parameter."
        | x -> x |> invalidParameter "'%s' invalid vector->string parameter."

    let sStringToVector envs cont =
        function
        | SString s :: _ as args ->
            match getVectorRange s.runes.Length args with
            | Some(start, stop) -> s.runes.[start .. stop - 1] |> Array.map SChar |> SVector |> cont
            | None -> args |> invalidParameter "'%s' invalid string->vector parameter."
        | x -> x |> invalidParameter "'%s' invalid string->vector parameter."

    let sVectorCopy envs cont =
        function
        | SVector xs :: _ as args ->
            match getVectorRange xs.Length args with
            | Some(start, stop) -> xs.[start .. stop - 1] |> Array.copy |> SVector |> cont
            | None -> args |> invalidParameter "'%s' invalid vector-copy parameter."
        | x -> x |> invalidParameter "'%s' invalid vector-copy parameter."

    let sVectorCopyBang envs cont =
        function
        | SVector target :: SRational(at, dAt) :: SVector source :: rest as args ->
            match getVectorRange source.Length (SVector source :: rest) with
            | Some(start, stop) when dAt = 1I && at >= 0I && at + bigint (stop - start) <= bigint target.Length ->
                Array.blit source start target (int at) (stop - start)
                SUnspecified |> cont
            | _ -> args |> invalidParameter "'%s' invalid vector-copy! parameter."
        | x -> x |> invalidParameter "'%s' invalid vector-copy! parameter."

    let sVectorAppend envs cont xs =
        xs
        |> List.map (function
            | SVector v -> v
            | x -> Print.print x |> sprintf "'%s' is not a vector." |> failwith)
        |> Array.concat
        |> SVector
        |> cont

    let sVectorFill envs cont =
        function
        | SVector xs :: fill :: rest as args ->
            match getVectorRange xs.Length (SVector xs :: rest) with
            | Some(start, stop) ->
                for i in start .. stop - 1 do
                    xs.[i] <- fill

                SUnspecified |> cont
            | None -> args |> invalidParameter "'%s' invalid vector-fill! parameter."
        | x -> x |> invalidParameter "'%s' invalid vector-fill! parameter."
