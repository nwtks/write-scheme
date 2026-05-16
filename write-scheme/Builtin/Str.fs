namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Str =
    let isString context pos cont =
        function
        | [ SString _, _ ] -> Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string? parameter." |> cont

    let sMakeString context pos cont =
        function
        | [ SRational(n, d), _ ] when d = 1I && n >= 0I ->
            Ok(
                ({ runes = Array.create (int n) (System.Text.Rune '\u0000')
                   isImmutable = false }
                 |> SString,
                 pos)
            )
            |> cont
        | [ SRational(n, d), _; SChar c, _ ] when d = 1I && n >= 0I ->
            Ok(
                ({ runes = Array.create (int n) c
                   isImmutable = false }
                 |> SString,
                 pos)
            )
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-string parameter." |> cont

    let sString context pos cont =
        mapResult (function
            | SChar c, _ -> Ok c
            | x -> x |> invalid (snd x) "'%s' is not a char in string.")
        >> Result.map (fun runes ->
            { runes = runes |> List.toArray
              isImmutable = false }
            |> SString,
            pos)
        >> cont

    let sStringLength context pos cont =
        function
        | [ SString s, _ ] -> Ok(bigint s.runes.Length |> newInteger, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string-length parameter." |> cont

    let sStringRef context pos cont =
        function
        | [ SString s, _; SRational(n, d), _ ] when d = 1I && n >= 0I && n < bigint s.runes.Length ->
            Ok(s.runes.[int n] |> SChar, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string-ref parameter." |> cont

    let sStringSetBang context pos cont =
        function
        | [ SString s, _; SRational(n, d), _; SChar c, _ ] when d = 1I && n >= 0I && n < bigint s.runes.Length ->
            if s.isImmutable then
                EvalError("Immutable string in string-set!.", pos) |> Error |> cont
            else
                s.runes.[int n] <- c
                Ok(SUnspecified, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string-set! parameter." |> cont

    let compareStringsBase transformer pred name pos cont =
        mapResult (function
            | SString s, _ -> Ok(s.runes |> runesToString |> transformer)
            | x -> x |> invalid (snd x) (sprintf "'%%s' is not a string in %s." name))
        >> Result.map (
            List.pairwise
            >> List.forall (fun (a, b) -> pred a b)
            >> toSBool
            >> fun x -> x, pos
        )
        >> cont

    let compareStrings pred = compareStringsBase id pred

    let compareStringsCi pred =
        compareStringsBase (fun s -> s.ToLowerInvariant()) pred

    let sStringEq context = compareStrings (=) "string=?"
    let sStringLt context = compareStrings (<) "string<?"
    let sStringGt context = compareStrings (>) "string>?"
    let sStringLe context = compareStrings (<=) "string<=?"
    let sStringGe context = compareStrings (>=) "string>=?"
    let sStringCiEq context = compareStringsCi (=) "string-ci=?"
    let sStringCiLt context = compareStringsCi (<) "string-ci<?"
    let sStringCiGt context = compareStringsCi (>) "string-ci>?"
    let sStringCiLe context = compareStringsCi (<=) "string-ci<=?"
    let sStringCiGe context = compareStringsCi (>=) "string-ci>=?"

    let sStringUpcase context pos cont =
        function
        | [ SString s, _ ] ->
            Ok((s.runes |> runesToString).ToUpperInvariant() |> newSString false, pos)
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string-upcase parameter." |> cont

    let sStringDowncase context pos cont =
        function
        | [ SString s, _ ] ->
            Ok((s.runes |> runesToString).ToLowerInvariant() |> newSString false, pos)
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string-downcase parameter." |> cont

    let sStringFoldcase context pos cont =
        function
        | [ SString s, _ ] ->
            Ok((s.runes |> runesToString).ToLowerInvariant() |> newSString false, pos)
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string-foldcase parameter." |> cont

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

    let sSubstring context pos cont args =
        match getRunesSlice args with
        | Some runes -> Ok({ runes = runes; isImmutable = false } |> SString, pos) |> cont
        | None -> args |> invalidParameter pos "'%s' invalid substring parameter." |> cont

    let sStringAppend context pos cont =
        mapResult (function
            | SString s, _ -> Ok(s.runes |> Array.toList)
            | x -> x |> invalid (snd x) "'%s' is not a string in string-append.")
        >> Result.map (fun runes ->
            { runes = runes |> List.concat |> List.toArray
              isImmutable = false }
            |> SString,
            pos)
        >> cont

    let sStringToList context pos cont args =
        match getRunesRange args with
        | Some(runes, start, count) ->
            runes.[start .. start + count - 1]
            |> Seq.map (fun c -> SChar c, pos)
            |> Seq.toList
            |> toSPair
            |> Ok
            |> cont
        | None -> args |> invalidParameter pos "'%s' invalid string->list parameter." |> cont

    let sListToString context pos cont =
        function
        | [ list ] when list |> isProperList ->
            list
            |> toList
            |> Result.bind (
                mapResult (function
                    | SChar c, _ -> Ok c
                    | x -> x |> invalid (snd x) "'%s' is not a char in list->string.")
            )
            |> Result.map (fun runes ->
                { runes = runes |> List.toArray
                  isImmutable = false }
                |> SString,
                pos)
            |> cont
        | [ SEmpty, _ ] -> Ok({ runes = [||]; isImmutable = false } |> SString, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid list->string parameter." |> cont

    let sStringCopy context pos cont args =
        match getRunesSlice args with
        | Some runes -> Ok({ runes = runes; isImmutable = false } |> SString, pos) |> cont
        | None -> args |> invalidParameter pos "'%s' invalid string-copy parameter." |> cont

    let sStringCopyBang context pos cont =
        function
        | (SString dest, _) :: (SRational(at, dAt), _) :: src as args when dAt = 1I && at >= 0I ->
            if dest.isImmutable then
                EvalError("Immutable destination string in string-copy!.", pos) |> Error |> cont
            else
                match getRunesRange src with
                | Some(runes, start, count) ->
                    if int at + count > dest.runes.Length then
                        EvalError("Destination out of range in string-copy!.", pos) |> Error |> cont
                    else
                        Array.blit runes start dest.runes (int at) count
                        Ok(SUnspecified, pos) |> cont
                | None -> args |> invalidParameter pos "'%s' invalid string-copy! parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string-copy! parameter." |> cont

    let sStringFillBang context pos cont =
        function
        | SString s, _ as str :: (SChar fill, _) :: range as args ->
            if s.isImmutable then
                EvalError("Immutable string in string-fill!.", pos) |> Error |> cont
            else
                match str :: range |> getRunesRange with
                | Some(runes, start, count) ->
                    Array.fill runes start count fill
                    Ok(SUnspecified, pos) |> cont
                | None -> args |> invalidParameter pos "'%s' invalid string-fill! parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string-fill! parameter." |> cont
