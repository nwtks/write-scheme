namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Port =
    let closePort p =
        p.isOpen <- false
        p.inputReader |> Option.iter (fun r -> r.Dispose())
        p.outputWriter |> Option.iter (fun w -> w.Dispose())
        p.fileStream |> Option.iter (fun s -> s.Dispose())

    let makeFilePort direction isTextual path =
        let stream =
            match direction with
            | Input -> System.IO.File.OpenRead path
            | Output -> System.IO.File.Create path

        { direction = direction
          isTextual = isTextual
          isOpen = true
          inputReader = None
          outputWriter = None
          fileStream = Some stream
          filePath = Some path }

    let makeInputStringPort path =
        let content = System.IO.File.ReadAllText path

        { direction = Input
          isTextual = true
          isOpen = true
          inputReader = Some(new System.IO.StringReader(content))
          outputWriter = None
          fileStream = None
          filePath = Some path }

    let openFileProc name direction isTextual =
        let fmt = sprintf "'%%s' invalid %s parameter." name

        fun context pos cont ->
            function
            | [ SString f, _ ] ->
                let path = f.runes |> runesToString

                try
                    makeFilePort direction isTextual path
                    |> SPort
                    |> fun p -> (p, pos) |> Ok |> cont
                with :? System.IO.IOException as ex ->
                    EvalError($"{name}: {ex.Message}", pos) |> Error |> cont
            | x -> x |> invalidParameter pos fmt |> cont

    let callWithFileProc name direction =
        let fmt = sprintf "'%%s' invalid %s parameter." name

        fun context pos cont ->
            function
            | [ SString f, _; proc ] ->
                let path = f.runes |> runesToString

                try
                    let port = makeFilePort direction true path
                    proc |> Eval.apply context cont [ SPort port, pos ]
                with :? System.IO.IOException as ex ->
                    EvalError($"{name}: {ex.Message}", pos) |> Error |> cont
            | x -> x |> invalidParameter pos fmt |> cont

    let withFileProc name makePort direction =
        let fmt = sprintf "'%%s' invalid %s parameter." name

        fun context pos cont ->
            function
            | [ SString f, _; proc ] ->
                let path = f.runes |> runesToString

                try
                    let port = makePort path

                    let savedPort, setPort =
                        match direction with
                        | Input -> context.ports.input, (fun p -> context.ports <- { context.ports with input = p })
                        | Output -> context.ports.output, (fun p -> context.ports <- { context.ports with output = p })

                    setPort port

                    let restore cont' result =
                        setPort savedPort
                        closePort port
                        cont' result

                    proc |> Eval.apply context (restore cont) []
                with :? System.IO.IOException as ex ->
                    EvalError($"{name}: {ex.Message}", pos) |> Error |> cont
            | x -> x |> invalidParameter pos fmt |> cont

    let closePortProc name =
        let fmt = sprintf "'%%s' invalid %s parameter." name

        fun context pos cont ->
            function
            | [ SPort p, _ ] ->
                closePort p
                (SUnspecified, pos) |> Ok |> cont
            | x -> x |> invalidParameter pos fmt |> cont

    let sCallWithPort context pos cont =
        function
        | [ SPort p, _; proc ] ->
            let closeAndCont result =
                closePort p
                result |> cont

            proc |> Eval.apply context closeAndCont [ SPort p, pos ]
        | [ arg; _ ] ->
            EvalError($"call-with-port: '{Print.print arg}' is not a port.", pos)
            |> Error
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid call-with-port parameter." |> cont

    let sCallWithInputFile: SProcedureKind =
        callWithFileProc "call-with-input-file" Input

    let sCallWithOutputFile: SProcedureKind =
        callWithFileProc "call-with-output-file" Output

    let isInputPort context pos cont =
        function
        | [ SPort p, _ ] -> (p.direction = Input |> toSBool, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid input-port? parameter." |> cont

    let isOutputPort context pos cont =
        function
        | [ SPort p, _ ] -> (p.direction = Output |> toSBool, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid output-port? parameter." |> cont

    let isTextualPort context pos cont =
        function
        | [ SPort p, _ ] -> (p.isTextual |> toSBool, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid textual-port? parameter." |> cont

    let isBinaryPort context pos cont =
        function
        | [ SPort p, _ ] -> (not p.isTextual |> toSBool, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid binary-port? parameter." |> cont

    let isPortExpr =
        function
        | SPort _, _ -> true
        | _ -> false

    let isPort context pos cont =
        function
        | [ x ] -> (isPortExpr x |> toSBool, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid port? parameter." |> cont

    let isInputPortOpen context pos cont =
        function
        | [ SPort p, _ ] -> ((p.direction = Input && p.isOpen) |> toSBool, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid input-port-open? parameter." |> cont

    let isOutputPortOpen context pos cont =
        function
        | [ SPort p, _ ] -> ((p.direction = Output && p.isOpen) |> toSBool, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid output-port-open? parameter." |> cont

    let sCurrentInputPort context pos cont =
        function
        | [] -> (SPort context.ports.input, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid current-input-port parameter." |> cont

    let sCurrentOutputPort context pos cont =
        function
        | [] -> (SPort context.ports.output, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid current-output-port parameter." |> cont

    let sCurrentErrorPort context pos cont =
        function
        | [] -> (SPort context.ports.error, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid current-error-port parameter." |> cont

    let sWithInputFromFile: SProcedureKind =
        withFileProc "with-input-from-file" makeInputStringPort Input

    let sWithOutputToFile: SProcedureKind =
        withFileProc "with-output-to-file" (makeFilePort Output true) Output

    let sOpenInputFile: SProcedureKind = openFileProc "open-input-file" Input true

    let sOpenBinaryInputFile: SProcedureKind =
        openFileProc "open-binary-input-file" Input false

    let sOpenOutputFile: SProcedureKind = openFileProc "open-output-file" Output true

    let sOpenBinaryOutputFile: SProcedureKind =
        openFileProc "open-binary-output-file" Output false

    let sClosePort: SProcedureKind = closePortProc "close-port"

    let sCloseInputPort: SProcedureKind = closePortProc "close-input-port"

    let sCloseOutputPort: SProcedureKind = closePortProc "close-output-port"

    let newInputStringPort s =
        { direction = Input
          isTextual = true
          isOpen = true
          inputReader = Some(new System.IO.StringReader(s))
          outputWriter = None
          fileStream = None
          filePath = None }

    let sOpenInputString context pos cont =
        function
        | [ SString s, _ ] ->
            s.runes
            |> runesToString
            |> newInputStringPort
            |> SPort
            |> fun x -> (x, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid open-input-string parameter." |> cont

    let newOutputStringPort () =
        { direction = Output
          isTextual = true
          isOpen = true
          inputReader = None
          outputWriter = Some(new System.IO.StringWriter())
          fileStream = None
          filePath = None }

    let sOpenOutputString context pos cont =
        function
        | [] -> newOutputStringPort () |> SPort |> (fun x -> x, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid open-output-string parameter." |> cont

    let sGetOutputString context pos cont =
        function
        | [ SPort p, _ ] ->
            match p.outputWriter with
            | Some w -> newSString false (w.ToString()) |> fun x -> (x, pos) |> Ok |> cont
            | None -> EvalError("get-output-string: not an output string port.", pos) |> Error |> cont
        | x -> x |> invalidParameter pos "'%s' invalid get-output-string parameter." |> cont

    let newInputBytevectorPort (bv: byte array) =
        { direction = Input
          isTextual = false
          isOpen = true
          inputReader = None
          outputWriter = None
          fileStream = Some(new System.IO.MemoryStream(bv))
          filePath = None }

    let sOpenInputBytevector context pos cont =
        function
        | [ SByteVector bv, _ ] -> newInputBytevectorPort bv |> SPort |> (fun x -> x, pos) |> Ok |> cont
        | x ->
            x
            |> invalidParameter pos "'%s' invalid open-input-bytevector parameter."
            |> cont

    let newOutputBytevectorPort () =
        { direction = Output
          isTextual = false
          isOpen = true
          inputReader = None
          outputWriter = None
          fileStream = Some(new System.IO.MemoryStream())
          filePath = None }

    let sOpenOutputBytevector context pos cont =
        function
        | [] -> newOutputBytevectorPort () |> SPort |> (fun x -> x, pos) |> Ok |> cont
        | x ->
            x
            |> invalidParameter pos "'%s' invalid open-output-bytevector parameter."
            |> cont

    let sGetOutputBytevector context pos cont =
        function
        | [ SPort p, _ ] ->
            match p.fileStream with
            | Some s ->
                let ms = s :?> System.IO.MemoryStream
                SByteVector(ms.ToArray()) |> fun x -> (x, pos) |> Ok |> cont
            | _ ->
                EvalError("get-output-bytevector: not an output bytevector port.", pos)
                |> Error
                |> cont
        | x ->
            x
            |> invalidParameter pos "'%s' invalid get-output-bytevector parameter."
            |> cont

    let readFromReader (r: System.IO.StringReader) pos cont =
        let line = r.ReadLine()

        if isNull line then
            (SEof, pos) |> Ok |> cont
        else
            match Read.read false (line |> runesToString) with
            | Ok e -> e |> Ok |> cont
            | Error(ParseError(msg, _)) -> EvalError(msg, pos) |> Error |> cont
            | Error _ -> EvalError("read error", pos) |> Error |> cont

    let sRead context pos cont =
        function
        | [] ->
            match context.ports.input.inputReader with
            | Some r -> readFromReader r pos cont
            | None -> (SEof, pos) |> Ok |> cont
        | [ SPort p, _ ] ->
            match p.inputReader with
            | Some r -> readFromReader r pos cont
            | None -> (SEof, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid read parameter." |> cont

    let readCharFromPort p =
        match p.inputReader with
        | Some r ->
            let c = r.Read()
            if c = -1 then None else Some(System.Text.Rune c)
        | None ->
            match p.fileStream with
            | Some s when not p.isTextual ->
                let b = s.ReadByte()
                if b = -1 then None else Some(System.Text.Rune b)
            | _ -> None

    let sReadChar context pos cont =
        function
        | [] ->
            let c = readCharFromPort context.ports.input

            match c with
            | Some r -> (SChar r, pos) |> Ok |> cont
            | None -> (SEof, pos) |> Ok |> cont
        | [ SPort p, _ ] ->
            let c = readCharFromPort p

            match c with
            | Some r -> (SChar r, pos) |> Ok |> cont
            | None -> (SEof, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid read-char parameter." |> cont

    let peekCharFromPort p =
        match p.inputReader with
        | Some r ->
            let c = r.Peek()
            if c = -1 then None else Some(System.Text.Rune c)
        | None ->
            match p.fileStream with
            | Some s when not p.isTextual ->
                let b = s.ReadByte()

                if b = -1 then
                    None
                else
                    s.Seek(-1L, System.IO.SeekOrigin.Current) |> ignore
                    Some(System.Text.Rune b)
            | _ -> None

    let sPeekChar context pos cont =
        function
        | [] ->
            let c = peekCharFromPort context.ports.input

            match c with
            | Some r -> (SChar r, pos) |> Ok |> cont
            | None -> (SEof, pos) |> Ok |> cont
        | [ SPort p, _ ] ->
            let c = peekCharFromPort p

            match c with
            | Some r -> (SChar r, pos) |> Ok |> cont
            | None -> (SEof, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid peek-char parameter." |> cont

    let sReadLine context pos cont =
        function
        | [] ->
            match context.ports.input.inputReader with
            | Some r ->
                let line = r.ReadLine()

                if isNull line then
                    (SEof, pos) |> Ok |> cont
                else
                    newSString false line |> fun x -> (x, pos) |> Ok |> cont
            | None -> (SEof, pos) |> Ok |> cont
        | [ SPort p, _ ] ->
            match p.inputReader with
            | Some r ->
                let line = r.ReadLine()

                if isNull line then
                    (SEof, pos) |> Ok |> cont
                else
                    newSString false line |> fun x -> (x, pos) |> Ok |> cont
            | None -> (SEof, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid read-line parameter." |> cont

    let isEofObject context pos cont =
        function
        | [ SEof, _ ] -> (STrue, pos) |> Ok |> cont
        | [ _ ] -> (SFalse, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid eof-object? parameter." |> cont

    let sEofObject context pos cont =
        function
        | [] -> (SEof, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid eof-object parameter." |> cont

    let isCharReady context pos cont =
        function
        | []
        | [ SPort _, _ ] -> (STrue, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid char-ready? parameter." |> cont

    let sReadString context pos cont =
        function
        | [ SRational(n, _), _ ] ->
            match context.ports.input.inputReader with
            | Some r ->
                let buffer = Array.zeroCreate<char> (int n)
                let count = r.Read(buffer, 0, int n)

                if count = 0 then
                    (SEof, pos) |> Ok |> cont
                else
                    newSString false (System.String(buffer, 0, count))
                    |> fun x -> (x, pos) |> Ok |> cont
            | None -> (SEof, pos) |> Ok |> cont
        | [ SRational(n, _), _; SPort p, _ ] ->
            match p.inputReader with
            | Some r ->
                let buffer = Array.zeroCreate<char> (int n)
                let count = r.Read(buffer, 0, int n)

                if count = 0 then
                    (SEof, pos) |> Ok |> cont
                else
                    newSString false (System.String(buffer, 0, count))
                    |> fun x -> (x, pos) |> Ok |> cont
            | None -> (SEof, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid read-string parameter." |> cont

    let peekU8FromPort p =
        match p.fileStream with
        | Some s when not p.isTextual ->
            let b = s.ReadByte()

            if b = -1 then
                None
            else
                s.Seek(-1L, System.IO.SeekOrigin.Current) |> ignore
                Some b
        | _ -> None

    let sReadU8 context pos cont =
        function
        | [] ->
            match context.ports.input.fileStream with
            | Some s when not context.ports.input.isTextual ->
                let b = s.ReadByte()

                if b = -1 then
                    (SEof, pos) |> Ok |> cont
                else
                    (SRational(bigint b, 1I), pos) |> Ok |> cont
            | _ -> (SEof, pos) |> Ok |> cont
        | [ SPort p, _ ] ->
            match p.fileStream with
            | Some s when not p.isTextual ->
                let b = s.ReadByte()

                if b = -1 then
                    (SEof, pos) |> Ok |> cont
                else
                    (SRational(bigint b, 1I), pos) |> Ok |> cont
            | _ -> (SEof, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid read-u8 parameter." |> cont

    let sPeekU8 context pos cont =
        function
        | [] ->
            match peekU8FromPort context.ports.input with
            | Some b -> (SRational(bigint b, 1I), pos) |> Ok |> cont
            | None -> (SEof, pos) |> Ok |> cont
        | [ SPort p, _ ] ->
            match peekU8FromPort p with
            | Some b -> (SRational(bigint b, 1I), pos) |> Ok |> cont
            | None -> (SEof, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid peek-u8 parameter." |> cont

    let isU8Ready context pos cont =
        function
        | []
        | [ SPort _, _ ] -> (STrue, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid u8-ready? parameter." |> cont

    let sReadBytevector context pos cont =
        function
        | [ SRational(n, _), _ ] ->
            match context.ports.input.fileStream with
            | Some s when not context.ports.input.isTextual ->
                let buffer = Array.zeroCreate<byte> (int n)
                let count = s.Read(buffer, 0, int n)

                if count = 0 then
                    (SEof, pos) |> Ok |> cont
                else
                    SByteVector buffer.[0 .. count - 1] |> fun x -> (x, pos) |> Ok |> cont
            | _ -> (SEof, pos) |> Ok |> cont
        | [ SRational(n, _), _; SPort p, _ ] ->
            match p.fileStream with
            | Some s when not p.isTextual ->
                let buffer = Array.zeroCreate<byte> (int n)
                let count = s.Read(buffer, 0, int n)

                if count = 0 then
                    (SEof, pos) |> Ok |> cont
                else
                    SByteVector buffer.[0 .. count - 1] |> fun x -> (x, pos) |> Ok |> cont
            | _ -> (SEof, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid read-bytevector parameter." |> cont

    let readBytevectorBangFromPort bv p startIdx endIdx pos cont =
        match p.fileStream with
        | Some s when not p.isTextual ->
            let count = s.Read(bv, startIdx, endIdx - startIdx)

            if count = 0 then
                (SEof, pos) |> Ok |> cont
            else
                (SRational(bigint count, 1I), pos) |> Ok |> cont
        | _ -> (SEof, pos) |> Ok |> cont

    let sReadBytevectorBang context pos cont =
        function
        | [ SByteVector bv, _ ] -> readBytevectorBangFromPort bv context.ports.input 0 bv.Length pos cont
        | [ SByteVector bv, _; SPort p, _ ] -> readBytevectorBangFromPort bv p 0 bv.Length pos cont
        | [ SByteVector bv, _; SPort p, _; SRational(startN, _), _ ] ->
            let startIdx = int startN

            if startIdx < 0 || startIdx > bv.Length then
                EvalError(
                    $"read-bytevector!: start index {startIdx} out of range for bytevector of length {bv.Length}.",
                    pos
                )
                |> Error
                |> cont
            else
                readBytevectorBangFromPort bv p startIdx bv.Length pos cont
        | [ SByteVector bv, _; SPort p, _; SRational(startN, _), _; SRational(endN, _), _ ] ->
            let startIdx = int startN
            let endIdx = int endN

            if startIdx < 0 || startIdx > bv.Length then
                EvalError(
                    $"read-bytevector!: start index {startIdx} out of range for bytevector of length {bv.Length}.",
                    pos
                )
                |> Error
                |> cont
            elif endIdx < 0 || endIdx > bv.Length then
                EvalError(
                    $"read-bytevector!: end index {endIdx} out of range for bytevector of length {bv.Length}.",
                    pos
                )
                |> Error
                |> cont
            elif startIdx > endIdx then
                EvalError($"read-bytevector!: start index {startIdx} is greater than end index {endIdx}.", pos)
                |> Error
                |> cont
            else
                readBytevectorBangFromPort bv p startIdx endIdx pos cont
        | (SByteVector _, _) :: rest ->
            let msg = rest |> List.map (fun arg -> Print.print arg) |> String.concat " "

            EvalError($"read-bytevector!: invalid argument(s) '{msg}'.", pos)
            |> Error
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid read-bytevector! parameter." |> cont

    let writeStringToPort p (s: string) =
        match p.outputWriter with
        | Some w -> w.Write s
        | None ->
            match p.fileStream with
            | Some fs when p.isTextual ->
                let bytes = System.Text.Encoding.UTF8.GetBytes s
                fs.Write(bytes, 0, bytes.Length)
            | _ -> ()

    let sWrite context pos cont =
        function
        | [ arg ] ->
            writeStringToPort context.ports.output (arg |> Print.print)
            (SUnspecified, pos) |> Ok |> cont
        | [ arg; SPort p, _ ] ->
            writeStringToPort p (arg |> Print.print)
            (SUnspecified, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid write parameter." |> cont

    let sWriteShared context pos cont =
        function
        | [ arg ] ->
            writeStringToPort context.ports.output (arg |> Print.printShared)
            (SUnspecified, pos) |> Ok |> cont
        | [ arg; SPort p, _ ] ->
            writeStringToPort p (arg |> Print.printShared)
            (SUnspecified, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid write-shared parameter." |> cont

    let sWriteSimple context pos cont =
        function
        | [ arg ] ->
            writeStringToPort context.ports.output (arg |> Print.print)
            (SUnspecified, pos) |> Ok |> cont
        | [ arg; SPort p, _ ] ->
            writeStringToPort p (arg |> Print.print)
            (SUnspecified, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid write-simple parameter." |> cont

    let getDisplayString =
        function
        | SString x, _ -> x.runes |> runesToString
        | SChar x, _ -> x |> string
        | expr -> expr |> Print.print

    let sDisplay context pos cont =
        function
        | [ arg ] ->
            writeStringToPort context.ports.output (arg |> getDisplayString)
            (SUnspecified, pos) |> Ok |> cont
        | [ arg; SPort p, _ ] ->
            writeStringToPort p (arg |> getDisplayString)
            (SUnspecified, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid display parameter." |> cont

    let sNewline context pos cont =
        function
        | [] ->
            writeStringToPort context.ports.output "\n"
            (SUnspecified, pos) |> Ok |> cont
        | [ SPort p, _ ] ->
            writeStringToPort p "\n"
            (SUnspecified, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid newline parameter." |> cont

    let sWriteChar context pos cont =
        function
        | [ SChar c, _ ] ->
            writeStringToPort context.ports.output (string c)
            (SUnspecified, pos) |> Ok |> cont
        | [ SChar c, _; SPort p, _ ] ->
            writeStringToPort p (string c)
            (SUnspecified, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid write-char parameter." |> cont

    let sWriteString context pos cont =
        function
        | [ SString s, _ ] ->
            let str = s.runes |> runesToString
            writeStringToPort context.ports.output str
            (SUnspecified, pos) |> Ok |> cont
        | (SString s, _) :: (SPort p, _) :: rest ->
            let str = s.runes |> runesToString

            match getRange str.Length rest with
            | Some(start, stop) ->
                writeStringToPort p str.[start .. stop - 1]
                (SUnspecified, pos) |> Ok |> cont
            | None ->
                let args = rest |> List.map Print.print |> String.concat " "
                EvalError($"write-string: invalid argument(s) '{args}'.", pos) |> Error |> cont
        | x -> x |> invalidParameter pos "'%s' invalid write-string parameter." |> cont

    let sWriteU8 context pos cont =
        function
        | [ SRational(n, _), _; SPort p, _ ] when not p.isTextual ->
            match p.fileStream with
            | Some fs -> fs.WriteByte(byte n)
            | None -> ()

            (SUnspecified, pos) |> Ok |> cont
        | [ SRational(n, _), _ ] ->
            match context.ports.output.fileStream with
            | Some fs when not context.ports.output.isTextual -> fs.WriteByte(byte n)
            | _ -> ()

            (SUnspecified, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid write-u8 parameter." |> cont

    let sWriteBytevector context pos cont =
        function
        | [ SByteVector bv, _; SPort p, _ ] when not p.isTextual ->
            match p.fileStream with
            | Some fs -> fs.Write(bv, 0, bv.Length)
            | None -> ()

            (SUnspecified, pos) |> Ok |> cont
        | [ SByteVector bv, _ ] ->
            match context.ports.output.fileStream with
            | Some fs when not context.ports.output.isTextual -> fs.Write(bv, 0, bv.Length)
            | _ -> ()

            (SUnspecified, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid write-bytevector parameter." |> cont

    let sFlushOutputPort context pos cont =
        function
        | [] ->
            match context.ports.output.outputWriter with
            | Some w -> w.Flush()
            | None -> ()

            (SUnspecified, pos) |> Ok |> cont
        | [ SPort p, _ ] ->
            match p.outputWriter with
            | Some w -> w.Flush()
            | None -> ()

            (SUnspecified, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid flush-output-port parameter." |> cont
