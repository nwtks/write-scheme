namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module ByteVector =
    let isByteVector context pos cont =
        function
        | [ SByteVector _, _ ] -> Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid bytevector? parameter." |> cont

    let sMakeByteVector context pos cont =
        function
        | [ SRational(n, d), _ ] when d = 1I && n >= 0I -> Ok(Array.create (int n) 0uy |> SByteVector, pos) |> cont
        | [ SRational(n, d), _; SRational(b, d'), _ ] when d = 1I && n >= 0I && d' = 1I && b >= 0I && b <= 255I ->
            Ok(Array.create (int n) (byte b) |> SByteVector, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-bytevector parameter." |> cont

    let sByteVector context pos cont =
        mapResult (function
            | SRational(b, d), _ when d = 1I && b >= 0I && b <= 255I -> Ok(byte b)
            | x -> x |> invalid (snd x) "'%s' invalid bytevector element.")
        >> Result.map (fun bs -> bs |> List.toArray |> SByteVector, pos)
        >> cont

    let sByteVectorLength context pos cont =
        function
        | [ SByteVector bytevector, _ ] -> Ok(bigint bytevector.Length |> newInteger, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid bytevector-length parameter." |> cont

    let sByteVectorU8Ref context pos cont =
        function
        | [ SByteVector bytevector, _; SRational(n, d), _ ] when d = 1I && n >= 0I && n < bigint bytevector.Length ->
            Ok(bigint bytevector.[int n] |> newInteger, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid bytevector-u8-ref parameter." |> cont

    let sByteVectorU8SetBang context pos cont =
        function
        | [ SByteVector bytevector, _; SRational(n, d), _; SRational(b, d'), _ ] when
            d = 1I
            && n >= 0I
            && n < bigint bytevector.Length
            && d' = 1I
            && b >= 0I
            && b <= 255I
            ->
            bytevector.[int n] <- byte b
            Ok(SUnspecified, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid bytevector-u8-set! parameter." |> cont

    let sByteVectorCopy context pos cont =
        function
        | (SByteVector bytevector, _) :: range as args ->
            match getRange bytevector.Length range with
            | Some(start, stop) -> Ok(bytevector.[start .. stop - 1] |> Array.copy |> SByteVector, pos) |> cont
            | None -> args |> invalidParameter pos "'%s' invalid bytevector-copy parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid bytevector-copy parameter." |> cont

    let sByteVectorCopyBang context pos cont =
        function
        | (SByteVector dest, _) :: (SRational(at, dAt), _) :: (SByteVector src, _) :: range as args ->
            match getRange src.Length range with
            | Some(start, stop) when dAt = 1I && at >= 0I && at + bigint (stop - start) <= bigint dest.Length ->
                Array.blit src start dest (int at) (stop - start)
                Ok(SUnspecified, pos) |> cont
            | _ -> args |> invalidParameter pos "'%s' invalid bytevector-copy! parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid bytevector-copy! parameter." |> cont

    let sByteVectorAppend context pos cont =
        mapResult (function
            | SByteVector bytevector, _ -> Ok bytevector
            | x -> x |> invalid (snd x) "'%s' is not a bytevector in bytevector-append.")
        >> Result.map (fun bytevectors -> bytevectors |> Array.concat |> SByteVector, pos)
        >> cont

    let sUtf8ToString context pos cont =
        function
        | (SByteVector bytevector, _) :: range as args ->
            match getRange bytevector.Length range with
            | Some(start, stop) ->
                bytevector.[start .. stop - 1]
                |> System.Text.Encoding.UTF8.GetString
                |> newSString false
                |> fun x -> Ok(x, pos)
                |> cont
            | None -> args |> invalidParameter pos "'%s' invalid utf8->string parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid utf8->string parameter." |> cont

    let runeRangeToString (start: int) (stop: int) (runes: System.Text.Rune array) =
        let sb = System.Text.StringBuilder(stop - start)
        runes.[start .. stop - 1] |> Seq.iter (string >> sb.Append >> ignore)
        sb |> string

    let sStringToUtf8 context pos cont =
        function
        | (SString s, _) :: range as args ->
            match getRange s.runes.Length range with
            | Some(start, stop) ->
                s.runes
                |> runeRangeToString start stop
                |> System.Text.Encoding.UTF8.GetBytes
                |> SByteVector
                |> fun x -> Ok(x, pos)
                |> cont
            | None -> args |> invalidParameter pos "'%s' invalid string->utf8 parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string->utf8 parameter." |> cont
