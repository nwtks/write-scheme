namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Vector =
    let isVector context pos cont =
        function
        | [ SVector _, _ ] -> Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector? parameter." |> cont

    let sMakeVector context pos cont =
        function
        | [ SRational(n, d), _ ] when d = 1I && n >= 0I ->
            Ok(Array.create (int n) (SUnspecified, pos) |> SVector, pos) |> cont
        | [ SRational(n, d), _; fill ] when d = 1I && n >= 0I -> Ok(Array.create (int n) fill |> SVector, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-vector parameter." |> cont

    let sVector context pos cont args =
        Ok(args |> List.toArray |> SVector, pos) |> cont

    let sVectorLength context pos cont =
        function
        | [ SVector vector, _ ] -> Ok(bigint vector.Length |> newInteger, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector-length parameter." |> cont

    let sVectorRef context pos cont =
        function
        | [ SVector vector, _; SRational(n, d), _ ] when d = 1I && n >= 0I && n < bigint vector.Length ->
            Ok vector.[int n] |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector-ref parameter." |> cont

    let sVectorSetBang context pos cont =
        function
        | [ SVector vector, _; SRational(n, d), _; obj ] when d = 1I && n >= 0I && n < bigint vector.Length ->
            vector.[int n] <- obj
            Ok(SUnspecified, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector-set! parameter." |> cont

    let sVectorToList context pos cont =
        function
        | (SVector vector, _) :: range as args ->
            match getRange vector.Length range with
            | Some(start, stop) -> Ok(vector.[start .. stop - 1] |> Array.toList |> toSPair) |> cont
            | None -> args |> invalidParameter pos "'%s' invalid vector->list parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector->list parameter." |> cont

    let sListToVector context pos cont =
        function
        | [ SEmpty, _ ] -> Ok(SVector [||], pos) |> cont
        | [ list ] when list |> isProperList ->
            list
            |> toList
            |> Result.map (fun l -> l |> List.toArray |> SVector, pos)
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid list->vector parameter." |> cont

    let sVectorToString context pos cont =
        function
        | (SVector vector, _) :: range as args ->
            match getRange vector.Length range with
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

    let sStringToVector context pos cont =
        function
        | (SString s, _) :: range as args ->
            match getRange s.runes.Length range with
            | Some(start, stop) ->
                Ok(s.runes.[start .. stop - 1] |> Array.map (fun c -> SChar c, pos) |> SVector, pos)
                |> cont
            | None -> args |> invalidParameter pos "'%s' invalid string->vector parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string->vector parameter." |> cont

    let sVectorCopy context pos cont =
        function
        | (SVector vector, _) :: range as args ->
            match getRange vector.Length range with
            | Some(start, stop) -> Ok(vector.[start .. stop - 1] |> Array.copy |> SVector, pos) |> cont
            | None -> args |> invalidParameter pos "'%s' invalid vector-copy parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector-copy parameter." |> cont

    let sVectorCopyBang context pos cont =
        function
        | (SVector dest, _) :: (SRational(at, dAt), _) :: (SVector src, _) :: range as args ->
            match getRange src.Length range with
            | Some(start, stop) when dAt = 1I && at >= 0I && at + bigint (stop - start) <= bigint dest.Length ->
                Array.blit src start dest (int at) (stop - start)
                Ok(SUnspecified, pos) |> cont
            | _ -> args |> invalidParameter pos "'%s' invalid vector-copy! parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector-copy! parameter." |> cont

    let sVectorAppend context pos cont =
        mapResult (function
            | SVector vector, _ -> Ok vector
            | x -> x |> invalid (snd x) "'%s' is not a vector in vector-append.")
        >> Result.map (fun vectors -> vectors |> Array.concat |> SVector, pos)
        >> cont

    let sVectorFillBang context pos cont =
        function
        | (SVector vector, _) :: fill :: range as args ->
            match getRange vector.Length range with
            | Some(start, stop) ->
                Array.fill vector start (stop - start) fill
                Ok(SUnspecified, pos) |> cont
            | None -> args |> invalidParameter pos "'%s' invalid vector-fill! parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector-fill! parameter." |> cont
