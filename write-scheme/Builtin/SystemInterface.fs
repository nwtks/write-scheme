namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module SystemInterface =
    let readAndEvalFile context f p =
        readAndResolveInclude false f p
        |> Result.bind (fun exprs' -> exprs' |> mapResult (Eval.eval context id))

    let sLoad context pos cont =
        function
        | [ SString f, p ] ->
            readAndEvalFile context f p
            |> Result.map (fun _ -> f.runes |> runesToString |> (fun s -> $"Loaded '{s}'.") |> SSymbol, pos)
            |> cont
        | [ SString f, p; SEnvironment env, _ ] ->
            let loadContext = { context with environments = [ env ] }

            readAndEvalFile loadContext f p
            |> Result.map (fun _ -> f.runes |> runesToString |> (fun s -> $"Loaded '{s}'.") |> SSymbol, pos)
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid load parameter." |> cont

    let isFileExists context pos cont =
        function
        | [ SString f, _ ] ->
            let path = f.runes |> runesToString
            (System.IO.File.Exists path |> toSBool, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid file-exists? parameter." |> cont

    let sDeleteFile context pos cont =
        function
        | [ SString f, _ ] ->
            let path = f.runes |> runesToString

            if System.IO.File.Exists path then
                System.IO.File.Delete path
                (SUnspecified, pos) |> Ok |> cont
            else
                let msg =
                    { runes = (sprintf "delete-file: file not found: %s" path).EnumerateRunes() |> Seq.toArray
                      isImmutable = false }

                SchemeRaise((SError(FileError, msg, []), pos), pos) |> Error |> cont
        | x -> x |> invalidParameter pos "'%s' invalid delete-file parameter." |> cont

    let sCommandLine context pos cont =
        function
        | [] ->
            context.commandLineArgs
            |> List.map (fun s -> s |> newSString true, pos)
            |> toSPair
            |> Ok
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid command-line parameter." |> cont

    [<TailCall>]
    let rec runExitWinders context pos cont code =
        function
        | [] ->
            System.Environment.Exit code
            failwith "unreachable."
        | w :: rest ->
            w.after
            |> Eval.apply
                context
                (function
                | Ok _ -> rest |> runExitWinders context pos cont code
                | Error _ -> rest |> runExitWinders context pos cont code)
                []

    let sExit context pos cont =
        function
        | [] -> context.winders.Value |> runExitWinders context pos cont 0
        | [ SBool false, _ ] -> context.winders.Value |> runExitWinders context pos cont 1
        | [ SBool true, _ ] -> context.winders.Value |> runExitWinders context pos cont 0
        | [ SRational(n, d), _ ] when d = 1I ->
            let code =
                if n >= 0I && n <= bigint System.Int32.MaxValue then
                    int n
                else
                    0

            context.winders.Value |> runExitWinders context pos cont code
        | x -> x |> invalidParameter pos "'%s' invalid exit parameter." |> cont

    let sEmergencyExit context pos cont =
        function
        | [] ->
            System.Environment.Exit 0
            failwith "unreachable."
        | [ SBool false, _ ] ->
            System.Environment.Exit 1
            failwith "unreachable."
        | [ SBool true, _ ] ->
            System.Environment.Exit 0
            failwith "unreachable."
        | [ SRational(n, d), _ ] when d = 1I ->
            let code =
                if n >= 0I && n <= bigint System.Int32.MaxValue then
                    int n
                else
                    0

            System.Environment.Exit code
            failwith "unreachable."
        | x -> x |> invalidParameter pos "'%s' invalid emergency-exit parameter." |> cont

    let sGetEnvironmentVariable context pos cont =
        function
        | [ SString name, _ ] ->
            let varName = name.runes |> runesToString

            match System.Environment.GetEnvironmentVariable varName with
            | null -> Ok(SFalse, pos) |> cont
            | value -> Ok(value |> newSString true, pos) |> cont
        | x ->
            x
            |> invalidParameter pos "'%s' invalid get-environment-variable parameter."
            |> cont

    let sGetEnvironmentVariables context pos cont =
        function
        | [] ->
            System.Environment.GetEnvironmentVariables()
            |> Seq.cast<System.Collections.DictionaryEntry>
            |> Seq.map (fun entry ->
                let name = entry.Key |> string |> newSString true, None
                let value = entry.Value |> string |> newSString true, None

                SPair
                    { car = name
                      cdr = SPair { car = value; cdr = SEmpty, None }, None },
                None)
            |> Seq.toList
            |> toSPair
            |> Ok
            |> cont
        | x ->
            x
            |> invalidParameter pos "'%s' invalid get-environment-variables parameter."
            |> cont
