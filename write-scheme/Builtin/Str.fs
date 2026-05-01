namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Str =
    let isString envs pos cont =
        function
        | [ SString _, _ ] -> (STrue, pos) |> cont
        | _ -> (SFalse, pos) |> cont

    let sMakeString envs pos cont =
        function
        | [ SRational(k, d), _ ] when d = 1I && k >= 0I ->
            { runes = Array.create (int k) (System.Text.Rune '\u0000')
              isImmutable = false }
            |> SString
            |> fun x -> x, pos
            |> cont
        | [ SRational(k, d), _; SChar c, _ ] when d = 1I && k >= 0I ->
            { runes = Array.create (int k) c
              isImmutable = false }
            |> SString
            |> fun x -> x, pos
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-string parameter."

    let sString envs pos cont xs =
        let runes =
            xs
            |> List.map (function
                | SChar c, _ -> c
                | x -> x |> invalid (snd x) "'%s' is not a char in string.")
            |> List.toArray

        ({ runes = runes; isImmutable = false } |> SString, pos) |> cont

    let sStringLength envs pos cont =
        function
        | [ SString s, _ ] -> (newSRational (bigint s.runes.Length) 1I, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string-length parameter."

    let sStringRef envs pos cont =
        function
        | [ SString s, _; SRational(k, d), _ ] when d = 1I && k >= 0I && k < bigint s.runes.Length ->
            (s.runes.[int k] |> SChar, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string-ref parameter."

    let sStringSetBang envs pos cont =
        function
        | [ SString s, _; SRational(k, d), _; SChar c, _ ] when d = 1I && k >= 0I && k < bigint s.runes.Length ->
            if s.isImmutable then
                failwithf "Immutable string in string-set!.%s" (pos |> formatPosition)

            s.runes.[int k] <- c
            (SUnspecified, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string-set! parameter."

    let compareStringsBase transformer pred name pos cont =
        List.map (function
            | SString s, _ -> s.runes |> runesToString |> transformer
            | x -> failwithf "'%s' is not a string in %s.%s" (x |> Print.print) name (x |> snd |> formatPosition))
        >> List.pairwise
        >> List.forall (fun (a, b) -> pred a b)
        >> toSBool
        >> fun x -> x, pos
        >> cont

    let compareStrings pred = compareStringsBase id pred

    let compareStringsCi pred =
        compareStringsBase (fun s -> s.ToLowerInvariant()) pred

    let sStringEq envs = compareStrings (=) "string=?"
    let sStringLt envs = compareStrings (<) "string<?"
    let sStringGt envs = compareStrings (>) "string>?"
    let sStringLe envs = compareStrings (<=) "string<=?"
    let sStringGe envs = compareStrings (>=) "string>=?"
    let sStringCiEq envs = compareStringsCi (=) "string-ci=?"
    let sStringCiLt envs = compareStringsCi (<) "string-ci<?"
    let sStringCiGt envs = compareStringsCi (>) "string-ci>?"
    let sStringCiLe envs = compareStringsCi (<=) "string-ci<=?"
    let sStringCiGe envs = compareStringsCi (>=) "string-ci>=?"

    let sStringUpcase envs pos cont =
        function
        | [ SString s, _ ] -> ((s.runes |> runesToString).ToUpperInvariant() |> newSString false, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string-upcase parameter."

    let sStringDowncase envs pos cont =
        function
        | [ SString s, _ ] -> ((s.runes |> runesToString).ToLowerInvariant() |> newSString false, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string-downcase parameter."

    let sStringFoldcase envs pos cont =
        function
        | [ SString s, _ ] -> ((s.runes |> runesToString).ToLowerInvariant() |> newSString false, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string-foldcase parameter."

    let getRunesRange =
        function
        | [ SString s, _ ] -> Some(s.runes, 0, s.runes.Length)
        | [ SString s, _; SRational(start, d), _ ] when d = 1I && start >= 0I && start <= bigint s.runes.Length ->
            Some(s.runes, int start, s.runes.Length - int start)
        | [ SString s, _; SRational(start, d1), _; SRational(stop, d2), _ ] when
            d1 = 1I
            && d2 = 1I
            && start >= 0I
            && stop >= start
            && stop <= bigint s.runes.Length
            ->
            Some(s.runes, int start, int stop - int start)
        | _ -> None

    let getRunesSlice =
        getRunesRange
        >> Option.map (fun (runes, start, count) -> Array.sub runes start count)

    let sSubstring envs pos cont args =
        match getRunesSlice args with
        | Some slice -> ({ runes = slice; isImmutable = false } |> SString, pos) |> cont
        | None -> args |> invalidParameter pos "'%s' invalid substring parameter."

    let sStringAppend envs pos cont xs =
        let runes =
            xs
            |> List.collect (function
                | SString s, _ -> s.runes |> Array.toList
                | x -> x |> invalid (snd x) "'%s' is not a string in string-append.")
            |> List.toArray

        ({ runes = runes; isImmutable = false } |> SString, pos) |> cont

    let sStringToList envs pos cont args =
        match getRunesRange args with
        | Some(runes, start, count) ->
            runes.[start .. start + count - 1]
            |> Seq.map (fun c -> SChar c, pos)
            |> Seq.toList
            |> toSPair
            |> cont
        | None -> args |> invalidParameter pos "'%s' invalid string->list parameter."

    let sListToString envs pos cont =
        function
        | [ x ] when isProperList x ->
            let runes =
                x
                |> toList
                |> List.map (function
                    | SChar c, _ -> c
                    | x -> x |> invalid (snd x) "'%s' is not a char in list->string.")
                |> List.toArray

            ({ runes = runes; isImmutable = false } |> SString, pos) |> cont
        | [ SEmpty, _ ] -> ({ runes = [||]; isImmutable = false } |> SString, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid list->string parameter."

    let sStringCopy envs pos cont args =
        match getRunesSlice args with
        | Some slice -> ({ runes = slice; isImmutable = false } |> SString, pos) |> cont
        | None -> args |> invalidParameter pos "'%s' invalid string-copy parameter."

    let sStringCopyBang envs pos cont =
        function
        | (SString dest, _) :: (SRational(at, dAt), _) :: rest as args when dAt = 1I && at >= 0I ->
            if dest.isImmutable then
                failwithf "Immutable destination string in string-copy!.%s" (pos |> formatPosition)

            match getRunesRange rest with
            | Some(srcRunes, srcStart, count) ->
                if int at + count > dest.runes.Length then
                    failwithf "Destination out of range in string-copy!.%s" (pos |> formatPosition)

                Array.blit srcRunes srcStart dest.runes (int at) count
                (SUnspecified, pos) |> cont
            | None -> args |> invalidParameter pos "'%s' invalid string-copy! parameter."
        | x -> x |> invalidParameter pos "'%s' invalid string-copy! parameter."

    let sStringFillBang envs pos cont =
        function
        | SString s, _ as str :: (SChar c, _) :: rest as args ->
            if s.isImmutable then
                failwithf "Immutable string in string-fill!.%s" (formatPosition pos)

            match str :: rest |> getRunesRange with
            | Some(runes, start, count) ->
                Array.fill runes start count c
                (SUnspecified, pos) |> cont
            | None -> args |> invalidParameter pos "'%s' invalid string-fill! parameter."
        | x -> x |> invalidParameter pos "'%s' invalid string-fill! parameter."
