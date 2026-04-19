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
        | [ SRational(k, d) ] when d = 1I && k >= 0I -> System.String('\u0000', int k) |> SString |> cont
        | [ SRational(k, d); SChar c ] when d = 1I && k >= 0I ->
            let result = System.Text.StringBuilder()

            for _ in 1I .. k do
                result.Append c |> ignore

            result.ToString() |> SString |> cont
        | x -> x |> invalidParameter "'%s' invalid make-string parameter."

    let sString envs cont xs =
        xs
        |> List.map (function
            | SChar c -> c
            | x -> Print.print x |> sprintf "'%s' is not a char." |> failwith)
        |> String.concat ""
        |> SString
        |> cont

    let sStringLength envs cont =
        function
        | [ SString s ] ->
            let count = s.EnumerateRunes() |> Seq.length
            newSRational (bigint count) 1I |> cont
        | x -> x |> invalidParameter "'%s' invalid string-length parameter."

    let toRunes (s: string) =
        s.EnumerateRunes() |> Seq.map (fun r -> r.ToString()) |> Seq.toArray

    let sStringRef envs cont =
        function
        | [ SString s; SRational(k, d) ] when d = 1I && k >= 0I ->
            let runes = toRunes s

            if k < bigint runes.Length then
                runes.[int k] |> SChar |> cont
            else
                failwith "string-ref: index out of range"
        | x -> x |> invalidParameter "'%s' invalid string-ref parameter."

    let compareStringsBase transformer pred name cont =
        function
        | []
        | [ _ ] -> STrue |> cont
        | args ->
            let strings =
                args
                |> List.map (function
                    | SString s -> transformer s
                    | x -> Print.print x |> sprintf "'%s' is not a string in %s." name |> failwith)

            strings
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
        | [ SString s ] -> s.ToUpperInvariant() |> SString |> cont
        | x -> x |> invalidParameter "'%s' invalid string-upcase parameter."

    let sStringDowncase envs cont =
        function
        | [ SString s ] -> s.ToLowerInvariant() |> SString |> cont
        | x -> x |> invalidParameter "'%s' invalid string-downcase parameter."

    let sStringFoldcase envs cont =
        function
        | [ SString s ] -> s.ToLowerInvariant() |> SString |> cont
        | x -> x |> invalidParameter "'%s' invalid string-foldcase parameter."

    let toRunesSlice name =
        function
        | [ SString s ] -> toRunes s |> Some
        | [ SString s; SRational(start, d) ] when d = 1I && start >= 0I ->
            let runes = toRunes s

            if start <= bigint runes.Length then
                runes.[int start ..] |> Some
            else
                failwithf "%s: start index out of range" name
        | [ SString s; SRational(start, d1); SRational(stop, d2) ] when
            d1 = 1I && d2 = 1I && start >= 0I && stop >= start
            ->
            let runes = toRunes s

            if stop <= bigint runes.Length then
                runes.[int start .. int stop - 1] |> Some
            else
                failwithf "%s: stop index out of range" name
        | _ -> None

    let sSubstring envs cont args =
        match toRunesSlice "substring" args with
        | Some slice -> slice |> String.concat "" |> SString |> cont
        | None -> args |> invalidParameter "'%s' invalid substring parameter."

    let sStringAppend envs cont xs =
        xs
        |> List.map (function
            | SString s -> s
            | x -> Print.print x |> sprintf "'%s' is not a string." |> failwith)
        |> String.concat ""
        |> SString
        |> cont

    let sStringToList envs cont args =
        match toRunesSlice "string->list" args with
        | Some slice -> slice |> Seq.map SChar |> Seq.toList |> toSList |> cont
        | None -> args |> invalidParameter "'%s' invalid string->list parameter."

    let sListToString envs cont =
        function
        | [ SList xs ] ->
            xs
            |> List.map (function
                | SChar c -> c
                | x -> Print.print x |> sprintf "'%s' is not a char." |> failwith)
            |> String.concat ""
            |> SString
            |> cont
        | [ SEmpty ] -> SString "" |> cont
        | x -> x |> invalidParameter "'%s' invalid list->string parameter."

    let sStringCopy envs cont args =
        match toRunesSlice "string-copy" args with
        | Some slice -> slice |> String.concat "" |> SString |> cont
        | None -> args |> invalidParameter "'%s' invalid string-copy parameter."
