namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module ByteVector =
    let isByteVector envs pos cont =
        function
        | [ SByteVector _, _ ] -> Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid bytevector? parameter." |> cont

    let sMakeByteVector envs pos cont =
        function
        | [ SRational(n, d), _ ] when d = 1I && n >= 0I -> Ok(Array.create (int n) 0uy |> SByteVector, pos) |> cont
        | [ SRational(n, d), _; SRational(b, d'), _ ] when d = 1I && n >= 0I && d' = 1I && b >= 0I && b <= 255I ->
            Ok(Array.create (int n) (byte b) |> SByteVector, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-bytevector parameter." |> cont

    let sByteVector envs pos cont =
        mapResult (function
            | SRational(b, d), _ when d = 1I && b >= 0I && b <= 255I -> Ok(byte b)
            | x -> x |> invalid (snd x) "'%s' invalid bytevector element.")
        >> Result.map (fun bs -> bs |> List.toArray |> SByteVector, pos)
        >> cont

    let sByteVectorLength envs pos cont =
        function
        | [ SByteVector bytevector, _ ] -> Ok(newInteger (bigint bytevector.Length), pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid bytevector-length parameter." |> cont

    let sByteVectorU8Ref envs pos cont =
        function
        | [ SByteVector bytevector, _; SRational(n, d), _ ] when d = 1I && n >= 0I && n < bigint bytevector.Length ->
            Ok(newInteger (bigint bytevector.[int n]), pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid bytevector-u8-ref parameter." |> cont

    let sByteVectorU8Set envs pos cont =
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

    let getByteVectorRange (length: int) =
        function
        | [ _ ] -> Some(0, length)
        | [ _; SRational(start, d), _ ] when d = 1I && start >= 0I && start <= bigint length -> Some(int start, length)
        | [ _; SRational(start, d1), _; SRational(stop, d2), _ ] when
            d1 = 1I && d2 = 1I && start >= 0I && stop >= start && stop <= bigint length
            ->
            Some(int start, int stop)
        | _ -> None

    let sByteVectorCopy envs pos cont =
        function
        | (SByteVector bytevector, _) :: _ as args ->
            match getByteVectorRange bytevector.Length args with
            | Some(start, stop) -> Ok(bytevector.[start .. stop - 1] |> Array.copy |> SByteVector, pos) |> cont
            | None -> args |> invalidParameter pos "'%s' invalid bytevector-copy parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid bytevector-copy parameter." |> cont

    let sByteVectorCopyBang envs pos cont =
        function
        | (SByteVector dest, _) :: (SRational(at, dAt), _) :: (SByteVector src, _) :: range as args ->
            match (SByteVector src, pos) :: range |> getByteVectorRange src.Length with
            | Some(start, stop) when dAt = 1I && at >= 0I && at + bigint (stop - start) <= bigint dest.Length ->
                Array.blit src start dest (int at) (stop - start)
                Ok(SUnspecified, pos) |> cont
            | _ -> args |> invalidParameter pos "'%s' invalid bytevector-copy! parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid bytevector-copy! parameter." |> cont

    let sByteVectorAppend envs pos cont =
        mapResult (function
            | SByteVector bytevector, _ -> Ok bytevector
            | x -> x |> invalid (snd x) "'%s' is not a bytevector in bytevector-append.")
        >> Result.map (fun vlist -> vlist |> Array.concat |> SByteVector, pos)
        >> cont

    let sUtf8ToString envs pos cont =
        function
        | (SByteVector bytevector, _) :: _ as args ->
            match getByteVectorRange bytevector.Length args with
            | Some(start, stop) ->
                bytevector.[start .. stop - 1]
                |> System.Text.Encoding.UTF8.GetString
                |> newSString false
                |> fun x -> Ok(x, pos)
                |> cont
            | None -> args |> invalidParameter pos "'%s' invalid utf8->string parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid utf8->string parameter." |> cont

    let sStringToUtf8 envs pos cont =
        function
        | (SString s, _) :: _ as args ->
            match getByteVectorRange s.runes.Length args with
            | Some(start, stop) ->
                let str = System.Text.StringBuilder stop

                for i in start .. stop - 1 do
                    s.runes.[i] |> string |> str.Append |> ignore

                Ok(str |> string |> System.Text.Encoding.UTF8.GetBytes |> SByteVector, pos)
                |> cont
            | None -> args |> invalidParameter pos "'%s' invalid string->utf8 parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string->utf8 parameter." |> cont
