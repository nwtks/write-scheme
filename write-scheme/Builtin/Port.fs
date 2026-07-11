namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Port =
    let closePort p =
        p.isOpen <- false

        p.outputWriter
        |> Option.iter (fun w ->
            w.Flush()
            w.Dispose())

        p.inputReader |> Option.iter (fun r -> r.Dispose())
        p.fileStream |> Option.iter (fun s -> s.Dispose())

    let makeFilePort direction isTextual path =
        let stream =
            match direction with
            | Input -> System.IO.File.OpenRead path
            | Output -> System.IO.File.Create path

        let reader =
            match direction, isTextual with
            | Input, true -> Some(new System.IO.StreamReader(stream) :> System.IO.TextReader)
            | _ -> None

        let writer =
            match direction, isTextual with
            | Output, true ->
                Some(new System.IO.StreamWriter(stream, System.Text.Encoding.UTF8, 1024, true) :> System.IO.TextWriter)
            | _ -> None

        { direction = direction
          isTextual = isTextual
          isOpen = true
          inputReader = reader
          outputWriter = writer
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

    let openFileProc name direction isTextual : SProcedureKind =
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

                    let closeAndCont result =
                        closePort port
                        result |> cont

                    proc |> Eval.apply context closeAndCont [ SPort port, pos ]
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

    let closePortProc name : SProcedureKind =
        let fmt = sprintf "'%%s' invalid %s parameter." name

        fun context pos cont ->
            function
            | [ SPort p, _ ] ->
                closePort p
                (SUnspecified, pos) |> Ok |> cont
            | x -> x |> invalidParameter pos fmt |> cont

    let inputPortProc name fn : SProcedureKind =
        let fmt = sprintf "'%%s' invalid %s parameter." name

        fun context pos cont ->
            function
            | [] -> fn context pos cont context.ports.input
            | [ SPort p, _ ] -> fn context pos cont p
            | x -> x |> invalidParameter pos fmt |> cont

    let outputPortProc name fn : SProcedureKind =
        let fmt = sprintf "'%%s' invalid %s parameter." name

        fun context pos cont ->
            function
            | [ arg ] -> fn context pos cont context.ports.output arg
            | [ arg; SPort p, _ ] -> fn context pos cont p arg
            | x -> x |> invalidParameter pos fmt |> cont

    let wrapPortPred name pred : SProcedureKind =
        let fmt = sprintf "'%%s' invalid %s parameter." name

        fun context pos cont ->
            function
            | [ SPort p, _ ] -> (pred p |> toSBool, pos) |> Ok |> cont
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

    let sCallWithInputFile = callWithFileProc "call-with-input-file" Input

    let sCallWithOutputFile = callWithFileProc "call-with-output-file" Output

    let isInputPort = wrapPortPred "input-port?" (fun p -> p.direction = Input)

    let isOutputPort = wrapPortPred "output-port?" (fun p -> p.direction = Output)

    let isTextualPort = wrapPortPred "textual-port?" (fun p -> p.isTextual)

    let isBinaryPort = wrapPortPred "binary-port?" (fun p -> not p.isTextual)

    let isPortExpr =
        function
        | SPort _, _ -> true
        | _ -> false

    let isPort context pos cont =
        function
        | [ x ] -> (isPortExpr x |> toSBool, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid port? parameter." |> cont

    let isInputPortOpen =
        wrapPortPred "input-port-open?" (fun p -> p.direction = Input && p.isOpen)

    let isOutputPortOpen =
        wrapPortPred "output-port-open?" (fun p -> p.direction = Output && p.isOpen)

    let currentPortProc name getPort : SProcedureKind =
        let fmt = sprintf "'%%s' invalid %s parameter." name

        fun context pos cont ->
            function
            | [] -> (SPort(getPort context.ports), pos) |> Ok |> cont
            | x -> x |> invalidParameter pos fmt |> cont

    let sCurrentInputPort = currentPortProc "current-input-port" (fun p -> p.input)

    let sCurrentOutputPort = currentPortProc "current-output-port" (fun p -> p.output)

    let sCurrentErrorPort = currentPortProc "current-error-port" (fun p -> p.error)

    let sWithInputFromFile =
        withFileProc "with-input-from-file" makeInputStringPort Input

    let sWithOutputToFile =
        withFileProc "with-output-to-file" (makeFilePort Output true) Output

    let sOpenInputFile = openFileProc "open-input-file" Input true

    let sOpenBinaryInputFile = openFileProc "open-binary-input-file" Input false

    let sOpenOutputFile = openFileProc "open-output-file" Output true

    let sOpenBinaryOutputFile = openFileProc "open-binary-output-file" Output false

    let sClosePort = closePortProc "close-port"

    let sCloseInputPort = closePortProc "close-input-port"

    let sCloseOutputPort = closePortProc "close-output-port"

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
            | Some(:? System.IO.StringWriter as sw) ->
                newSString false (sw.ToString()) |> fun x -> (x, pos) |> Ok |> cont
            | _ -> EvalError("get-output-string: not an output string port.", pos) |> Error |> cont
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

    let readFromReader (r: System.IO.TextReader) pos cont =
        let line = r.ReadLine()

        if isNull line then
            (SEof, pos) |> Ok |> cont
        else
            match Read.read false (line |> runesToString) with
            | Ok e -> e |> Ok |> cont
            | Error(ParseError(msg, _)) -> EvalError(msg, pos) |> Error |> cont
            | Error _ -> EvalError("read error", pos) |> Error |> cont

    let sRead =
        inputPortProc "read" (fun _ pos cont p ->
            match p.inputReader with
            | Some r -> readFromReader r pos cont
            | None -> (SEof, pos) |> Ok |> cont)

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

    let sReadChar =
        inputPortProc "read-char" (fun _ pos cont p ->
            match readCharFromPort p with
            | Some r -> (SChar r, pos) |> Ok |> cont
            | None -> (SEof, pos) |> Ok |> cont)

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

    let sPeekChar =
        inputPortProc "peek-char" (fun _ pos cont p ->
            match peekCharFromPort p with
            | Some r -> (SChar r, pos) |> Ok |> cont
            | None -> (SEof, pos) |> Ok |> cont)

    let sReadLine =
        inputPortProc "read-line" (fun _ pos cont p ->
            match p.inputReader with
            | Some r ->
                let line = r.ReadLine()

                if isNull line then
                    (SEof, pos) |> Ok |> cont
                else
                    newSString false line |> fun x -> (x, pos) |> Ok |> cont
            | None -> (SEof, pos) |> Ok |> cont)

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

    let readStringFromReader (r: System.IO.TextReader option) n pos cont =
        match r with
        | Some rdr ->
            let buffer = Array.zeroCreate<char> (int n)
            let count = rdr.Read(buffer, 0, int n)

            if count = 0 then
                (SEof, pos) |> Ok |> cont
            else
                newSString false (System.String(buffer, 0, count))
                |> fun x -> (x, pos) |> Ok |> cont
        | None -> (SEof, pos) |> Ok |> cont

    [<TailCall>]
    let rec sReadString context pos cont =
        function
        | [ SRational(_, _) as num, _ ] -> sReadString context pos cont [ num, None; SPort context.ports.input, None ]
        | [ SRational(n, d), _; SPort p, _ ] when d = 1I && n >= 0I -> readStringFromReader p.inputReader n pos cont
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

    let sReadU8 =
        inputPortProc "read-u8" (fun _ pos cont p ->
            match p.fileStream with
            | Some s when not p.isTextual ->
                let b = s.ReadByte()

                if b = -1 then
                    (SEof, pos) |> Ok |> cont
                else
                    (SRational(bigint b, 1I), pos) |> Ok |> cont
            | _ -> (SEof, pos) |> Ok |> cont)

    let sPeekU8 =
        inputPortProc "peek-u8" (fun _ pos cont p ->
            match peekU8FromPort p with
            | Some b -> (SRational(bigint b, 1I), pos) |> Ok |> cont
            | None -> (SEof, pos) |> Ok |> cont)

    let isU8Ready context pos cont =
        function
        | []
        | [ SPort _, _ ] -> (STrue, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid u8-ready? parameter." |> cont

    let readBytevectorFromStream (s: System.IO.Stream) n pos cont =
        let buffer = Array.zeroCreate<byte> (int n)
        let count = s.Read(buffer, 0, int n)

        if count = 0 then
            (SEof, pos) |> Ok |> cont
        else
            SByteVector buffer.[0 .. count - 1] |> fun x -> (x, pos) |> Ok |> cont

    [<TailCall>]
    let rec sReadBytevector context pos cont =
        function
        | [ SRational(_, _) as num, _ ] ->
            sReadBytevector context pos cont [ num, None; SPort context.ports.input, None ]
        | [ SRational(n, d), _; SPort p, _ ] when d = 1I && n >= 0I ->
            match p.fileStream with
            | Some s when not p.isTextual -> readBytevectorFromStream s n pos cont
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

    let withValidBvRange bvLength startN endN pos cont f =
        let startIdx = int startN
        let endIdx = int endN

        if startIdx < 0 || startIdx > bvLength then
            EvalError(
                $"read-bytevector!: start index {startIdx} out of range for bytevector of length {bvLength}.",
                pos
            )
            |> Error
            |> cont
        elif endIdx < 0 || endIdx > bvLength then
            EvalError($"read-bytevector!: end index {endIdx} out of range for bytevector of length {bvLength}.", pos)
            |> Error
            |> cont
        elif startIdx > endIdx then
            EvalError($"read-bytevector!: start index {startIdx} is greater than end index {endIdx}.", pos)
            |> Error
            |> cont
        else
            f startIdx endIdx

    [<TailCall>]
    let rec sReadBytevectorBang context pos cont =
        function
        | [ SByteVector bv, _ ] ->
            sReadBytevectorBang context pos cont [ SByteVector bv, None; SPort context.ports.input, None ]
        | [ SByteVector bv, _; SPort p, _ ] ->
            sReadBytevectorBang context pos cont [ SByteVector bv, None; SPort p, None; SZero, None ]
        | [ SByteVector bv, _; SPort p, _; SRational(startN, d), _ ] when d = 1I ->
            withValidBvRange bv.Length startN (bigint bv.Length) pos cont (fun st en ->
                readBytevectorBangFromPort bv p st en pos cont)
        | [ SByteVector bv, _; SPort p, _; SRational(startN, d), _; SRational(endN, d'), _ ] when d = 1I && d' = 1I ->
            withValidBvRange bv.Length startN endN pos cont (fun st en ->
                readBytevectorBangFromPort bv p st en pos cont)
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

    let sWrite =
        outputPortProc "write" (fun _ pos cont p arg ->
            writeStringToPort p (Print.print arg)
            (SUnspecified, pos) |> Ok |> cont)

    let sWriteShared =
        outputPortProc "write-shared" (fun _ pos cont p arg ->
            writeStringToPort p (Print.printShared arg)
            (SUnspecified, pos) |> Ok |> cont)

    let sWriteSimple =
        outputPortProc "write-simple" (fun _ pos cont p arg ->
            writeStringToPort p (Print.print arg)
            (SUnspecified, pos) |> Ok |> cont)

    let getDisplayString =
        function
        | SString x, _ -> x.runes |> runesToString
        | SChar x, _ -> x |> string
        | expr -> expr |> Print.print

    let sDisplay =
        outputPortProc "display" (fun _ pos cont p arg ->
            writeStringToPort p (arg |> getDisplayString)
            (SUnspecified, pos) |> Ok |> cont)

    let sNewline =
        inputPortProc "newline" (fun _ pos cont p ->
            writeStringToPort p "\n"
            (SUnspecified, pos) |> Ok |> cont)

    let sWriteChar =
        outputPortProc "write-char" (fun _ pos cont p arg ->
            match arg with
            | SChar c, _ ->
                writeStringToPort p (string c)
                (SUnspecified, pos) |> Ok |> cont
            | x -> EvalError($"write-char: '{Print.print x}' is not a char.", pos) |> Error |> cont)

    [<TailCall>]
    let rec sWriteString context pos cont =
        function
        | [ SString s, _ ] -> sWriteString context pos cont [ SString s, None; SPort context.ports.output, None ]
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

    [<TailCall>]
    let rec sWriteU8 context pos cont =
        function
        | [ SRational(_, _) as num, _ ] -> sWriteU8 context pos cont [ num, None; SPort context.ports.output, None ]
        | [ SRational(n, d), _; SPort p, _ ] when d = 1I && n >= 0I && n <= 255I && not p.isTextual ->
            p.fileStream |> Option.iter (fun fs -> fs.WriteByte(byte n))
            (SUnspecified, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid write-u8 parameter." |> cont

    [<TailCall>]
    let rec sWriteBytevector context pos cont =
        function
        | [ SByteVector bv, _ ] ->
            sWriteBytevector context pos cont [ SByteVector bv, None; SPort context.ports.output, None ]
        | [ SByteVector bv, _; SPort p, _ ] when not p.isTextual ->
            p.fileStream |> Option.iter (fun fs -> fs.Write(bv, 0, bv.Length))
            (SUnspecified, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid write-bytevector parameter." |> cont

    let sFlushOutputPort =
        inputPortProc "flush-output-port" (fun _ pos cont p ->
            p.outputWriter |> Option.iter (fun w -> w.Flush())
            (SUnspecified, pos) |> Ok |> cont)
