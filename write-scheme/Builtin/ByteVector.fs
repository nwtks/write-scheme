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
        let arr =
            xs
            |> List.map (function
                | SRational(num, den) when den = 1I && num >= 0I && num <= 255I -> byte num
                | x -> Print.print x |> sprintf "'%s' invalid bytevector element." |> failwith)
            |> List.toArray

        SByteVector arr |> cont

    let sByteVectorLength envs cont =
        function
        | [ SByteVector xs ] -> SRational(bigint xs.Length, 1I) |> cont
        | x -> x |> invalidParameter "'%s' invalid bytevector-length parameter."

    let sByteVectorU8Ref envs cont =
        function
        | [ SByteVector xs; SRational(k, d) ] when d = 1I && k >= 0I && k < bigint xs.Length ->
            SRational(bigint xs.[int k], 1I) |> cont
        | x -> x |> invalidParameter "'%s' invalid bytevector-u8-ref parameter."

    let sByteVectorU8Set envs cont =
        function
        | [ SByteVector xs; SRational(k, d); SRational(b, d') ] when
            d = 1I && k >= 0I && k < bigint xs.Length && d' = 1I && b >= 0I && b <= 255I
            ->
            xs.[int k] <- byte b
            SUnspecified |> cont
        | x -> x |> invalidParameter "'%s' invalid bytevector-u8-set! parameter."

    let sByteVectorCopy envs cont =
        function
        | [ SByteVector xs ] -> Array.copy xs |> SByteVector |> cont
        | x -> x |> invalidParameter "'%s' invalid bytevector-copy parameter."
