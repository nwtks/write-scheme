namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module ByteVector =
    let isByteVector envs pos cont =
        function
        | [ SByteVector _, _ ] -> (STrue, pos) |> cont
        | [ _ ] -> (SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid bytevector? parameter."

    let sMakeByteVector envs pos cont =
        function
        | [ SRational(k, d), _ ] when d = 1I && k >= 0I -> (Array.create (int k) 0uy |> SByteVector, pos) |> cont
        | [ SRational(k, d), _; SRational(b, d'), _ ] when d = 1I && k >= 0I && d' = 1I && b >= 0I && b <= 255I ->
            (Array.create (int k) (byte b) |> SByteVector, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-bytevector parameter."

    let sByteVector envs pos cont =
        List.map (function
            | SRational(num, den), _ when den = 1I && num >= 0I && num <= 255I -> byte num
            | x -> x |> invalid (snd x) "'%s' invalid bytevector element.")
        >> List.toArray
        >> SByteVector
        >> fun x -> x, pos
        >> cont

    let sByteVectorLength envs pos cont =
        function
        | [ SByteVector xs, _ ] -> (newSRational (bigint xs.Length) 1I, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid bytevector-length parameter."

    let sByteVectorU8Ref envs pos cont =
        function
        | [ SByteVector xs, _; SRational(k, d), _ ] when d = 1I && k >= 0I && k < bigint xs.Length ->
            (newSRational (bigint xs.[int k]) 1I, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid bytevector-u8-ref parameter."

    let sByteVectorU8Set envs pos cont =
        function
        | [ SByteVector xs, _; SRational(k, d), _; SRational(b, d'), _ ] when
            d = 1I && k >= 0I && k < bigint xs.Length && d' = 1I && b >= 0I && b <= 255I
            ->
            xs.[int k] <- byte b
            (SUnspecified, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid bytevector-u8-set! parameter."

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
        | (SByteVector xs, _) :: _ as args ->
            match getByteVectorRange xs.Length args with
            | Some(start, stop) -> (xs.[start .. stop - 1] |> Array.copy |> SByteVector, pos) |> cont
            | None -> args |> invalidParameter pos "'%s' invalid bytevector-copy parameter."
        | x -> x |> invalidParameter pos "'%s' invalid bytevector-copy parameter."

    let sByteVectorCopyBang envs pos cont =
        function
        | (SByteVector target, _) :: (SRational(at, dAt), _) :: (SByteVector source, _ as src) :: rest as args ->
            match getByteVectorRange source.Length (src :: rest) with
            | Some(start, stop) when dAt = 1I && at >= 0I && at + bigint (stop - start) <= bigint target.Length ->
                Array.blit source start target (int at) (stop - start)
                (SUnspecified, pos) |> cont
            | _ -> args |> invalidParameter pos "'%s' invalid bytevector-copy! parameter."
        | x -> x |> invalidParameter pos "'%s' invalid bytevector-copy! parameter."

    let sByteVectorAppend envs pos cont =
        List.map (function
            | SByteVector v, _ -> v
            | x -> x |> invalid (snd x) "'%s' is not a bytevector in bytevector-append.")
        >> Array.concat
        >> SByteVector
        >> fun x -> x, pos
        >> cont

    let sUtf8ToString envs pos cont =
        function
        | (SByteVector bs, _) :: _ as args ->
            match getByteVectorRange bs.Length args with
            | Some(start, stop) ->
                bs.[start .. stop - 1]
                |> System.Text.Encoding.UTF8.GetString
                |> newSString false
                |> fun x -> x, pos
                |> cont
            | None -> args |> invalidParameter pos "'%s' invalid utf8->string parameter."
        | x -> x |> invalidParameter pos "'%s' invalid utf8->string parameter."

    let sStringToUtf8 envs pos cont =
        function
        | (SString s, _) :: _ as args ->
            match getByteVectorRange s.runes.Length args with
            | Some(start, stop) ->
                let subStr = System.Text.StringBuilder stop

                for i in start .. stop - 1 do
                    s.runes.[i] |> string |> subStr.Append |> ignore

                (subStr |> string |> System.Text.Encoding.UTF8.GetBytes |> SByteVector, pos)
                |> cont
            | None -> args |> invalidParameter pos "'%s' invalid string->utf8 parameter."
        | x -> x |> invalidParameter pos "'%s' invalid string->utf8 parameter."
