namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Vector =
    let isVector envs pos cont =
        function
        | [ SVector _, _ ] -> Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector? parameter." |> cont

    let sMakeVector envs pos cont =
        function
        | [ SRational(n, d), _ ] when d = 1I && n >= 0I ->
            Ok(Array.create (int n) (SUnspecified, pos) |> SVector, pos) |> cont
        | [ SRational(n, d), _; fill ] when d = 1I && n >= 0I -> Ok(Array.create (int n) fill |> SVector, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-vector parameter." |> cont

    let sVector envs pos cont args =
        Ok(args |> List.toArray |> SVector, pos) |> cont

    let sVectorLength envs pos cont =
        function
        | [ SVector vector, _ ] -> Ok(bigint vector.Length |> newInteger, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector-length parameter." |> cont

    let sVectorRef envs pos cont =
        function
        | [ SVector vector, _; SRational(n, d), _ ] when d = 1I && n >= 0I && n < bigint vector.Length ->
            Ok vector.[int n] |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector-ref parameter." |> cont

    let sVectorSetBang envs pos cont =
        function
        | [ SVector vector, _; SRational(n, d), _; obj ] when d = 1I && n >= 0I && n < bigint vector.Length ->
            vector.[int n] <- obj
            Ok(SUnspecified, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector-set! parameter." |> cont

    let getVectorRange (length: int) =
        function
        | [] -> Some(0, length)
        | [ SRational(start, d), _ ] when d = 1I && start >= 0I && start <= bigint length -> Some(int start, length)
        | [ SRational(start, d1), _; SRational(stop, d2), _ ] when
            d1 = 1I && d2 = 1I && start >= 0I && stop >= start && stop <= bigint length
            ->
            Some(int start, int stop)
        | _ -> None

    let sVectorToList envs pos cont =
        function
        | (SVector vector, _) :: range as args ->
            match getVectorRange vector.Length range with
            | Some(start, stop) -> Ok(vector.[start .. stop - 1] |> Array.toList |> toSPair) |> cont
            | None -> args |> invalidParameter pos "'%s' invalid vector->list parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector->list parameter." |> cont

    let sListToVector envs pos cont =
        function
        | [ SEmpty, _ ] -> Ok(SVector [||], pos) |> cont
        | [ list ] when isProperList list ->
            list
            |> toList
            |> Result.map (fun l -> l |> List.toArray |> SVector, pos)
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid list->vector parameter." |> cont

    let sVectorToString envs pos cont =
        function
        | (SVector vector, _) :: range as args ->
            match getVectorRange vector.Length range with
            | Some(start, stop) ->
                vector.[start .. stop - 1]
                |> Array.toList
                |> mapResult (function
                    | SChar c, _ -> Ok c
                    | x -> x |> invalid (snd x) "'%s' is not a char in vector->string.")
                |> Result.map (fun runes ->
                    { runes = runes |> List.toArray
                      isImmutable = false }
                    |> SString,
                    pos)
                |> cont
            | None -> args |> invalidParameter pos "'%s' invalid vector->string parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector->string parameter." |> cont

    let sStringToVector envs pos cont =
        function
        | (SString s, _) :: range as args ->
            match getVectorRange s.runes.Length range with
            | Some(start, stop) ->
                Ok(s.runes.[start .. stop - 1] |> Array.map (fun c -> SChar c, pos) |> SVector, pos)
                |> cont
            | None -> args |> invalidParameter pos "'%s' invalid string->vector parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string->vector parameter." |> cont

    let sVectorCopy envs pos cont =
        function
        | (SVector vector, _) :: range as args ->
            match getVectorRange vector.Length range with
            | Some(start, stop) -> Ok(vector.[start .. stop - 1] |> Array.copy |> SVector, pos) |> cont
            | None -> args |> invalidParameter pos "'%s' invalid vector-copy parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector-copy parameter." |> cont

    let sVectorCopyBang envs pos cont =
        function
        | (SVector dest, _) :: (SRational(at, dAt), _) :: (SVector src, _) :: range as args ->
            match getVectorRange src.Length range with
            | Some(start, stop) when dAt = 1I && at >= 0I && at + bigint (stop - start) <= bigint dest.Length ->
                Array.blit src start dest (int at) (stop - start)
                Ok(SUnspecified, pos) |> cont
            | _ -> args |> invalidParameter pos "'%s' invalid vector-copy! parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector-copy! parameter." |> cont

    let sVectorAppend envs pos cont =
        mapResult (function
            | SVector vector, _ -> Ok vector
            | x -> x |> invalid (snd x) "'%s' is not a vector in vector-append.")
        >> Result.map (fun vectors -> vectors |> Array.concat |> SVector, pos)
        >> cont

    let sVectorFillBang envs pos cont =
        function
        | (SVector vector, _) :: fill :: range as args ->
            match getVectorRange vector.Length range with
            | Some(start, stop) ->
                for i in start .. stop - 1 do
                    vector.[i] <- fill

                Ok(SUnspecified, pos) |> cont
            | None -> args |> invalidParameter pos "'%s' invalid vector-fill! parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector-fill! parameter." |> cont
