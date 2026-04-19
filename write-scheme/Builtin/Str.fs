namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Str =
    let isString envs cont =
        function
        | [ SString _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    let sMakeString envs cont =
        function
        | [ SRational(k, d) ] when d = 1I && k >= 0I ->
            { runes = Array.create (int k) (System.Text.Rune '\u0000')
              isImmutable = false }
            |> SString
            |> cont
        | [ SRational(k, d); SChar c ] when d = 1I && k >= 0I ->
            { runes = Array.create (int k) c
              isImmutable = false }
            |> SString
            |> cont
        | x -> x |> invalidParameter "'%s' invalid make-string parameter."

    let sString envs cont xs =
        let runes =
            xs
            |> List.map (function
                | SChar c -> c
                | x -> Print.print x |> sprintf "'%s' is not a char." |> failwith)
            |> List.toArray

        { runes = runes; isImmutable = false } |> SString |> cont

    let sStringLength envs cont =
        function
        | [ SString s ] -> newSRational (bigint s.runes.Length) 1I |> cont
        | x -> x |> invalidParameter "'%s' invalid string-length parameter."

    let sStringRef envs cont =
        function
        | [ SString s; SRational(k, d) ] when d = 1I && k >= 0I && k < bigint s.runes.Length ->
            s.runes.[int k] |> SChar |> cont
        | x -> x |> invalidParameter "'%s' invalid string-ref parameter."

    let sStringSetBang envs cont =
        function
        | [ SString s; SRational(k, d); SChar c ] when d = 1I && k >= 0I && k < bigint s.runes.Length ->
            if s.isImmutable then
                failwith "string-set!: immutable string"

            s.runes.[int k] <- c
            SUnspecified |> cont
        | x -> x |> invalidParameter "'%s' invalid string-set! parameter."

    let compareStringsBase transformer pred name cont args =
        args
        |> List.map (function
            | SString s -> s.runes |> runesToString |> transformer
            | x -> Print.print x |> sprintf "'%s' is not a string in %s." name |> failwith)
        |> List.pairwise
        |> List.forall (fun (a, b) -> pred a b)
        |> toSBool
        |> cont

    let compareStrings pred name cont args =
        compareStringsBase id pred name cont args

    let compareStringsCi pred name cont args =
        compareStringsBase (fun s -> s.ToLowerInvariant()) pred name cont args

    let sStringEq envs cont = compareStrings (=) "string=?" cont
    let sStringLt envs cont = compareStrings (<) "string<?" cont
    let sStringGt envs cont = compareStrings (>) "string>?" cont
    let sStringLe envs cont = compareStrings (<=) "string<=?" cont
    let sStringGe envs cont = compareStrings (>=) "string>=?" cont
    let sStringCiEq envs cont = compareStringsCi (=) "string-ci=?" cont
    let sStringCiLt envs cont = compareStringsCi (<) "string-ci<?" cont
    let sStringCiGt envs cont = compareStringsCi (>) "string-ci>?" cont

    let sStringCiLe envs cont =
        compareStringsCi (<=) "string-ci<=?" cont

    let sStringCiGe envs cont =
        compareStringsCi (>=) "string-ci>=?" cont

    let sStringUpcase envs cont =
        function
        | [ SString s ] -> (s.runes |> runesToString).ToUpperInvariant() |> newSString false |> cont
        | x -> x |> invalidParameter "'%s' invalid string-upcase parameter."

    let sStringDowncase envs cont =
        function
        | [ SString s ] -> (s.runes |> runesToString).ToLowerInvariant() |> newSString false |> cont
        | x -> x |> invalidParameter "'%s' invalid string-downcase parameter."

    let sStringFoldcase envs cont =
        function
        | [ SString s ] -> (s.runes |> runesToString).ToLowerInvariant() |> newSString false |> cont
        | x -> x |> invalidParameter "'%s' invalid string-foldcase parameter."

    let getRunesRange =
        function
        | [ SString s ] -> Some(s.runes, 0, s.runes.Length)
        | [ SString s; SRational(start, d) ] when d = 1I && start >= 0I && start <= bigint s.runes.Length ->
            Some(s.runes, int start, s.runes.Length - int start)
        | [ SString s; SRational(start, d1); SRational(stop, d2) ] when
            d1 = 1I
            && d2 = 1I
            && start >= 0I
            && stop >= start
            && stop <= bigint s.runes.Length
            ->
            Some(s.runes, int start, int stop - int start)
        | _ -> None

    let getRunesSlice args =
        args
        |> getRunesRange
        |> Option.map (fun (runes, start, count) -> Array.sub runes start count)

    let sSubstring envs cont args =
        match getRunesSlice args with
        | Some slice -> { runes = slice; isImmutable = false } |> SString |> cont
        | None -> args |> invalidParameter "'%s' invalid substring parameter."

    let sStringAppend envs cont xs =
        let runes =
            xs
            |> List.collect (function
                | SString s -> s.runes |> Array.toList
                | x -> Print.print x |> sprintf "'%s' is not a string." |> failwith)
            |> List.toArray

        { runes = runes; isImmutable = false } |> SString |> cont

    let sStringToList envs cont args =
        match getRunesRange args with
        | Some(runes, start, count) ->
            runes.[start .. start + count - 1]
            |> Seq.map SChar
            |> Seq.toList
            |> toSList
            |> cont
        | None -> args |> invalidParameter "'%s' invalid string->list parameter."

    let sListToString envs cont =
        function
        | [ SList xs ] ->
            let runes =
                xs
                |> List.map (function
                    | SChar c -> c
                    | x -> Print.print x |> sprintf "'%s' is not a char." |> failwith)
                |> List.toArray

            { runes = runes; isImmutable = false } |> SString |> cont
        | [ SEmpty ] -> { runes = [||]; isImmutable = false } |> SString |> cont
        | x -> x |> invalidParameter "'%s' invalid list->string parameter."

    let sStringCopy envs cont args =
        match getRunesSlice args with
        | Some slice -> { runes = slice; isImmutable = false } |> SString |> cont
        | None -> args |> invalidParameter "'%s' invalid string-copy parameter."

    let sStringCopyBang envs cont =
        function
        | SString dest :: SRational(at, dAt) :: rest as args when dAt = 1I && at >= 0I ->
            if dest.isImmutable then
                failwith "string-copy!: immutable destination string"

            match getRunesRange rest with
            | Some(srcRunes, srcStart, count) ->
                if int at + count > dest.runes.Length then
                    failwith "string-copy!: destination out of range"

                Array.blit srcRunes srcStart dest.runes (int at) count
                SUnspecified |> cont
            | None -> args |> invalidParameter "'%s' invalid string-copy! parameter."
        | x -> x |> invalidParameter "'%s' invalid string-copy! parameter."

    let sStringFillBang envs cont =
        function
        | SString s :: SChar c :: rest as args ->
            if s.isImmutable then
                failwith "string-fill!: immutable string"

            match getRunesRange (SString s :: rest) with
            | Some(runes, start, count) ->
                Array.fill runes start count c
                SUnspecified |> cont
            | None -> args |> invalidParameter "'%s' invalid string-fill! parameter."
        | x -> x |> invalidParameter "'%s' invalid string-fill! parameter."
