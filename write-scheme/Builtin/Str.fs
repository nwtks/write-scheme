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
        | [ SRational(k, d); SChar c ] when d = 1I && k >= 0I -> System.String(c.[0], int k) |> SString |> cont
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
        | [ SString s ] -> SRational(bigint s.Length, 1I) |> cont
        | x -> x |> invalidParameter "'%s' invalid string-length parameter."

    let sStringRef envs cont =
        function
        | [ SString s; SRational(k, d) ] when d = 1I && k >= 0I && k < bigint s.Length ->
            SChar(string s.[int k]) |> cont
        | x -> x |> invalidParameter "'%s' invalid string-ref parameter."

    let compareStrings (pred: string -> string -> bool) name envs cont =
        function
        | []
        | [ _ ] -> STrue |> cont
        | args ->
            let strings =
                args
                |> List.map (function
                    | SString s -> s
                    | x -> Print.print x |> sprintf "'%s' is not a string in %s." name |> failwith)

            strings
            |> List.pairwise
            |> List.forall (fun (a, b) -> pred a b)
            |> toSBool
            |> cont

    let sStringEq envs cont = compareStrings (=) "string=?" envs cont
    let sStringLt envs cont = compareStrings (<) "string<?" envs cont
    let sStringGt envs cont = compareStrings (>) "string>?" envs cont

    let sStringLe envs cont =
        compareStrings (<=) "string<=?" envs cont

    let sStringGe envs cont =
        compareStrings (>=) "string>=?" envs cont

    let compareStringsCi (pred: string -> string -> bool) name envs cont =
        function
        | []
        | [ _ ] -> STrue |> cont
        | args ->
            let strings =
                args
                |> List.map (function
                    | SString s -> s.ToLowerInvariant()
                    | x -> Print.print x |> sprintf "'%s' is not a string in %s." name |> failwith)

            strings
            |> List.pairwise
            |> List.forall (fun (a, b) -> pred a b)
            |> toSBool
            |> cont

    let sStringCiEq envs cont =
        compareStringsCi (=) "string-ci=?" envs cont

    let sStringCiLt envs cont =
        compareStringsCi (<) "string-ci<?" envs cont

    let sStringCiGt envs cont =
        compareStringsCi (>) "string-ci>?" envs cont

    let sStringCiLe envs cont =
        compareStringsCi (<=) "string-ci<=?" envs cont

    let sStringCiGe envs cont =
        compareStringsCi (>=) "string-ci>=?" envs cont

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

    let sSubstring envs cont =
        function
        | [ SString s; SRational(start, d1) ] when d1 = 1I && start >= 0I && start <= bigint s.Length ->
            s.[int start ..] |> SString |> cont
        | [ SString s; SRational(start, d1); SRational(stop, d2) ] when
            d1 = 1I && d2 = 1I && start >= 0I && stop >= start && stop <= bigint s.Length
            ->
            s.[int start .. int stop - 1] |> SString |> cont
        | x -> x |> invalidParameter "'%s' invalid substring parameter."

    let sStringAppend envs cont xs =
        xs
        |> List.map (function
            | SString s -> s
            | x -> Print.print x |> sprintf "'%s' is not a string." |> failwith)
        |> String.concat ""
        |> SString
        |> cont

    let stringToSChars s = [ for c in s -> SChar(string c) ]

    let sStringToList envs cont =
        function
        | [ SString s ] -> s |> stringToSChars |> toSList |> cont
        | [ SString s; SRational(start, d1) ] when d1 = 1I && start >= 0I && start <= bigint s.Length ->
            s.[int start ..] |> stringToSChars |> toSList |> cont
        | [ SString s; SRational(start, d1); SRational(stop, d2) ] when
            d1 = 1I && d2 = 1I && start >= 0I && stop >= start && stop <= bigint s.Length
            ->
            s.[int start .. int stop - 1] |> stringToSChars |> toSList |> cont
        | x -> x |> invalidParameter "'%s' invalid string->list parameter."

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

    let sStringCopy envs cont =
        function
        | [ SString s ] -> SString s |> cont
        | [ SString s; SRational(start, d1) ] when d1 = 1I && start >= 0I && start <= bigint s.Length ->
            s.[int start ..] |> SString |> cont
        | [ SString s; SRational(start, d1); SRational(stop, d2) ] when
            d1 = 1I && d2 = 1I && start >= 0I && stop >= start && stop <= bigint s.Length
            ->
            s.[int start .. int stop - 1] |> SString |> cont
        | x -> x |> invalidParameter "'%s' invalid string-copy parameter."
