namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module ByteVector =
    let isByteVector envs cont =
        function
        | [ SByteVector _ ] -> STrue |> cont
        | [ _ ] -> SFalse |> cont
        | x -> x |> invalidParameter "'%s' invalid bytevector? parameter."

    let sMakeByteVector envs cont =
        function
        | [ SRational(k, d) ] when d = 1I && k >= 0I -> Array.create (int k) 0uy |> SByteVector |> cont
        | [ SRational(k, d); SRational(b, d') ] when d = 1I && k >= 0I && d' = 1I && b >= 0I && b <= 255I ->
            Array.create (int k) (byte b) |> SByteVector |> cont
        | x -> x |> invalidParameter "'%s' invalid make-bytevector parameter."

    let sByteVector envs cont xs =
        xs
        |> List.map (function
            | SRational(num, den) when den = 1I && num >= 0I && num <= 255I -> byte num
            | x -> Print.print x |> sprintf "'%s' invalid bytevector element." |> failwith)
        |> List.toArray
        |> SByteVector
        |> cont

    let sByteVectorLength envs cont =
        function
        | [ SByteVector xs ] -> newSRational (bigint xs.Length) 1I |> cont
        | x -> x |> invalidParameter "'%s' invalid bytevector-length parameter."

    let sByteVectorU8Ref envs cont =
        function
        | [ SByteVector xs; SRational(k, d) ] when d = 1I && k >= 0I && k < bigint xs.Length ->
            newSRational (bigint xs.[int k]) 1I |> cont
        | x -> x |> invalidParameter "'%s' invalid bytevector-u8-ref parameter."

    let sByteVectorU8Set envs cont =
        function
        | [ SByteVector xs; SRational(k, d); SRational(b, d') ] when
            d = 1I && k >= 0I && k < bigint xs.Length && d' = 1I && b >= 0I && b <= 255I
            ->
            xs.[int k] <- byte b
            SUnspecified |> cont
        | x -> x |> invalidParameter "'%s' invalid bytevector-u8-set! parameter."

    let getRange (length: int) =
        function
        | [ _ ] -> Some(0, length)
        | [ _; SRational(start, d) ] when d = 1I && start >= 0I && start <= bigint length -> Some(int start, length)
        | [ _; SRational(start, d1); SRational(stop, d2) ] when
            d1 = 1I && d2 = 1I && start >= 0I && stop >= start && stop <= bigint length
            ->
            Some(int start, int stop)
        | _ -> None

    let sByteVectorCopy envs cont =
        function
        | [ SByteVector xs ]
        | [ SByteVector xs; _ ]
        | [ SByteVector xs; _; _ ] as args ->
            match getRange xs.Length args with
            | Some(start, stop) -> xs.[start .. stop - 1] |> Array.copy |> SByteVector |> cont
            | None -> args |> invalidParameter "'%s' invalid bytevector-copy parameter."
        | x -> x |> invalidParameter "'%s' invalid bytevector-copy parameter."

    let sByteVectorCopyBang envs cont =
        function
        | [ SByteVector target; SRational(at, dAt); SByteVector source ]
        | [ SByteVector target; SRational(at, dAt); SByteVector source; _ ]
        | [ SByteVector target; SRational(at, dAt); SByteVector source; _; _ ] as args ->
            match getRange source.Length args.[2..] with
            | Some(start, stop) when dAt = 1I && at >= 0I && at + bigint (stop - start) <= bigint target.Length ->
                Array.blit source start target (int at) (stop - start)
                SUnspecified |> cont
            | _ -> args |> invalidParameter "'%s' invalid bytevector-copy! parameter."
        | x -> x |> invalidParameter "'%s' invalid bytevector-copy! parameter."

    let sByteVectorAppend envs cont xs =
        xs
        |> List.map (function
            | SByteVector v -> v
            | x -> Print.print x |> sprintf "'%s' is not a bytevector." |> failwith)
        |> Array.concat
        |> SByteVector
        |> cont

    let sUtf8ToString envs cont =
        function
        | [ SByteVector bs ]
        | [ SByteVector bs; _ ]
        | [ SByteVector bs; _; _ ] as args ->
            match getRange bs.Length args with
            | Some(start, stop) -> System.Text.Encoding.UTF8.GetString(bs.[start .. stop - 1]) |> SString |> cont
            | None -> args |> invalidParameter "'%s' invalid utf8->string parameter."
        | x -> x |> invalidParameter "'%s' invalid utf8->string parameter."

    let sStringToUtf8 envs cont =
        function
        | [ SString s ]
        | [ SString s; _ ]
        | [ SString s; _; _ ] as args ->
            let runes = s.EnumerateRunes() |> Seq.toArray

            match getRange runes.Length args with
            | Some(start, stop) ->
                let subStr = System.Text.StringBuilder()

                for i in start .. stop - 1 do
                    subStr.Append(runes.[i].ToString()) |> ignore

                System.Text.Encoding.UTF8.GetBytes(subStr.ToString())
                |> Array.map byte
                |> SByteVector
                |> cont
            | None -> args |> invalidParameter "'%s' invalid string->utf8 parameter."
        | x -> x |> invalidParameter "'%s' invalid string->utf8 parameter."
