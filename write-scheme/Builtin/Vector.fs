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

    let sVectorToList envs cont =
        function
        | [ SVector xs ] -> xs |> Array.toList |> toSList |> cont
        | [ SVector xs; SRational(start, d1) ] when d1 = 1I && start >= 0I && start <= bigint xs.Length ->
            xs.[int start ..] |> Array.toList |> toSList |> cont
        | [ SVector xs; SRational(start, d1); SRational(stop, d2) ] when
            d1 = 1I && d2 = 1I && start >= 0I && stop >= start && stop <= bigint xs.Length
            ->
            xs.[int start .. int stop - 1] |> Array.toList |> toSList |> cont
        | x -> x |> invalidParameter "'%s' invalid vector->list parameter."

    let sListToVector envs cont =
        function
        | [ SList xs ] -> xs |> List.toArray |> SVector |> cont
        | [ SEmpty ] -> [||] |> SVector |> cont
        | x -> x |> invalidParameter "'%s' invalid list->vector parameter."

    let sVectorToString envs cont =
        function
        | [ SVector xs ] ->
            xs
            |> Array.map (function
                | SChar c -> c
                | x -> Print.print x |> sprintf "'%s' is not a char." |> failwith)
            |> String.concat ""
            |> SString
            |> cont
        | [ SVector xs; SRational(start, d1) ] when d1 = 1I && start >= 0I && start <= bigint xs.Length ->
            xs.[int start ..]
            |> Array.map (function
                | SChar c -> c
                | x -> Print.print x |> sprintf "'%s' is not a char." |> failwith)
            |> String.concat ""
            |> SString
            |> cont
        | [ SVector xs; SRational(start, d1); SRational(stop, d2) ] when
            d1 = 1I && d2 = 1I && start >= 0I && stop >= start && stop <= bigint xs.Length
            ->
            xs.[int start .. int stop - 1]
            |> Array.map (function
                | SChar c -> c
                | x -> Print.print x |> sprintf "'%s' is not a char." |> failwith)
            |> String.concat ""
            |> SString
            |> cont
        | x -> x |> invalidParameter "'%s' invalid vector->string parameter."

    let sStringToVector envs cont =
        function
        | [ SString s ] -> s |> Seq.map (fun c -> SChar(string c)) |> Seq.toArray |> SVector |> cont
        | [ SString s; SRational(start, d1) ] when d1 = 1I && start >= 0I && start <= bigint s.Length ->
            s.[int start ..]
            |> Seq.map (fun c -> SChar(string c))
            |> Seq.toArray
            |> SVector
            |> cont
        | [ SString s; SRational(start, d1); SRational(stop, d2) ] when
            d1 = 1I && d2 = 1I && start >= 0I && stop >= start && stop <= bigint s.Length
            ->
            s.[int start .. int stop - 1]
            |> Seq.map (fun c -> SChar(string c))
            |> Seq.toArray
            |> SVector
            |> cont
        | x -> x |> invalidParameter "'%s' invalid string->vector parameter."

    let sVectorCopy envs cont =
        function
        | [ SVector xs ] -> xs |> Array.copy |> SVector |> cont
        | [ SVector xs; SRational(start, d1) ] when d1 = 1I && start >= 0I && start <= bigint xs.Length ->
            xs.[int start ..] |> Array.copy |> SVector |> cont
        | [ SVector xs; SRational(start, d1); SRational(stop, d2) ] when
            d1 = 1I && d2 = 1I && start >= 0I && stop >= start && stop <= bigint xs.Length
            ->
            xs.[int start .. int stop - 1] |> Array.copy |> SVector |> cont
        | x -> x |> invalidParameter "'%s' invalid vector-copy parameter."

    let sVectorCopyBang envs cont =
        function
        | [ SVector target; SRational(at, dAt); SVector source ] when
            dAt = 1I && at >= 0I && at + bigint source.Length <= bigint target.Length
            ->
            Array.blit source 0 target (int at) source.Length
            SUnspecified |> cont
        | [ SVector target; SRational(at, dAt); SVector source; SRational(start, d1) ] when
            dAt = 1I
            && d1 = 1I
            && start >= 0I
            && start <= bigint source.Length
            && at >= 0I
            && at + bigint (source.Length - int start) <= bigint target.Length
            ->
            let len = source.Length - int start
            Array.blit source (int start) target (int at) len
            SUnspecified |> cont
        | [ SVector target; SRational(at, dAt); SVector source; SRational(start, d1); SRational(stop, d2) ] when
            dAt = 1I
            && d1 = 1I
            && d2 = 1I
            && start >= 0I
            && stop >= start
            && stop <= bigint source.Length
            && at >= 0I
            && at + bigint (int stop - int start) <= bigint target.Length
            ->
            let len = int stop - int start
            Array.blit source (int start) target (int at) len
            SUnspecified |> cont
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
        | [ SVector xs; fill ] ->
            for i in 0 .. xs.Length - 1 do
                xs.[i] <- fill

            SUnspecified |> cont
        | [ SVector xs; fill; SRational(start, d1) ] when d1 = 1I && start >= 0I && start <= bigint xs.Length ->
            for i in int start .. xs.Length - 1 do
                xs.[i] <- fill

            SUnspecified |> cont
        | [ SVector xs; fill; SRational(start, d1); SRational(stop, d2) ] when
            d1 = 1I && d2 = 1I && start >= 0I && stop >= start && stop <= bigint xs.Length
            ->
            for i in int start .. int stop - 1 do
                xs.[i] <- fill

            SUnspecified |> cont
        | x -> x |> invalidParameter "'%s' invalid vector-fill! parameter."
