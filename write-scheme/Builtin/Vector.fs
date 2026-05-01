namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Vector =
    let isVector envs pos cont =
        function
        | [ SVector _, _ ] -> (STrue, pos) |> cont
        | [ _ ] -> (SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector? parameter."

    let sMakeVector envs pos cont =
        function
        | [ SRational(k, d), _ ] when d = 1I && k >= 0I ->
            (Array.create (int k) (SUnspecified, pos) |> SVector, pos) |> cont
        | [ SRational(k, d), _; fill ] when d = 1I && k >= 0I -> (Array.create (int k) fill |> SVector, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-vector parameter."

    let sVector envs pos cont =
        List.toArray >> SVector >> (fun x -> x, pos) >> cont

    let sVectorLength envs pos cont =
        function
        | [ SVector xs, _ ] -> (newSRational (bigint xs.Length) 1I, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector-length parameter."

    let sVectorRef envs pos cont =
        function
        | [ SVector xs, _; SRational(k, d), _ ] when d = 1I && k >= 0I && k < bigint xs.Length -> xs.[int k] |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector-ref parameter."

    let sVectorSet envs pos cont =
        function
        | [ SVector xs, _; SRational(k, d), _; obj ] when d = 1I && k >= 0I && k < bigint xs.Length ->
            xs.[int k] <- obj
            (SUnspecified, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector-set! parameter."

    let getVectorRange (length: int) =
        function
        | [ _ ] -> Some(0, length)
        | [ _; SRational(start, d), _ ] when d = 1I && start >= 0I && start <= bigint length -> Some(int start, length)
        | [ _; SRational(start, d1), _; SRational(stop, d2), _ ] when
            d1 = 1I && d2 = 1I && start >= 0I && stop >= start && stop <= bigint length
            ->
            Some(int start, int stop)
        | _ -> None

    let sVectorToList envs pos cont =
        function
        | (SVector xs, _) :: _ as args ->
            match getVectorRange xs.Length args with
            | Some(start, stop) -> xs.[start .. stop - 1] |> Array.toList |> toSPair |> cont
            | None -> args |> invalidParameter pos "'%s' invalid vector->list parameter."
        | x -> x |> invalidParameter pos "'%s' invalid vector->list parameter."

    let sListToVector envs pos cont =
        function
        | [ SEmpty, _ ] -> (SVector [||], pos) |> cont
        | [ x ] when isProperList x -> (x |> toList |> List.toArray |> SVector, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid list->vector parameter."

    let sVectorToString envs pos cont =
        function
        | (SVector xs, _) :: _ as args ->
            match getVectorRange xs.Length args with
            | Some(start, stop) ->
                let runes =
                    xs.[start .. stop - 1]
                    |> Array.map (function
                        | SChar c, _ -> c
                        | x -> x |> invalid (snd x) "'%s' is not a char in vector->string.")

                ({ runes = runes; isImmutable = false } |> SString, pos) |> cont
            | None -> args |> invalidParameter pos "'%s' invalid vector->string parameter."
        | x -> x |> invalidParameter pos "'%s' invalid vector->string parameter."

    let sStringToVector envs pos cont =
        function
        | (SString s, _) :: _ as args ->
            match getVectorRange s.runes.Length args with
            | Some(start, stop) ->
                (s.runes.[start .. stop - 1] |> Array.map (fun c -> SChar c, pos) |> SVector, pos)
                |> cont
            | None -> args |> invalidParameter pos "'%s' invalid string->vector parameter."
        | x -> x |> invalidParameter pos "'%s' invalid string->vector parameter."

    let sVectorCopy envs pos cont =
        function
        | (SVector xs, _) :: _ as args ->
            match getVectorRange xs.Length args with
            | Some(start, stop) -> (xs.[start .. stop - 1] |> Array.copy |> SVector, pos) |> cont
            | None -> args |> invalidParameter pos "'%s' invalid vector-copy parameter."
        | x -> x |> invalidParameter pos "'%s' invalid vector-copy parameter."

    let sVectorCopyBang envs pos cont =
        function
        | (SVector target, _) :: (SRational(at, dAt), _) :: (SVector source, _) :: rest as args ->
            match getVectorRange source.Length ((SVector source, pos) :: rest) with
            | Some(start, stop) when dAt = 1I && at >= 0I && at + bigint (stop - start) <= bigint target.Length ->
                Array.blit source start target (int at) (stop - start)
                (SUnspecified, pos) |> cont
            | _ -> args |> invalidParameter pos "'%s' invalid vector-copy! parameter."
        | x -> x |> invalidParameter pos "'%s' invalid vector-copy! parameter."

    let sVectorAppend envs pos cont =
        List.map (function
            | SVector v, _ -> v
            | x -> x |> invalid (snd x) "'%s' is not a vector in vector-append.")
        >> Array.concat
        >> SVector
        >> fun x -> x, pos
        >> cont

    let sVectorFill envs pos cont =
        function
        | (SVector xs, _) :: fill :: rest as args ->
            match getVectorRange xs.Length ((SVector xs, pos) :: rest) with
            | Some(start, stop) ->
                for i in start .. stop - 1 do
                    xs.[i] <- fill

                (SUnspecified, pos) |> cont
            | None -> args |> invalidParameter pos "'%s' invalid vector-fill! parameter."
        | x -> x |> invalidParameter pos "'%s' invalid vector-fill! parameter."
