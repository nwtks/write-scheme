namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Port =
    let closePort p =
        p.isOpen <- false

        match p.inputReader with
        | Some r -> r.Dispose()
        | None -> ()

        match p.outputWriter with
        | Some w -> w.Dispose()
        | None -> ()

        match p.fileStream with
        | Some s -> s.Dispose()
        | None -> ()

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

    let sCallWithInputFile context pos cont =
        function
        | [ SString f, _; proc ] ->
            let path = f.runes |> runesToString

            try
                let stream = System.IO.File.OpenRead path

                let port =
                    { direction = Input
                      isTextual = true
                      isOpen = true
                      inputReader = None
                      outputWriter = None
                      fileStream = Some stream
                      filePath = Some path }

                proc |> Eval.apply context cont [ SPort port, pos ]
            with :? System.IO.FileNotFoundException as ex ->
                EvalError($"call-with-input-file: {ex.Message}", pos) |> Error |> cont
        | x -> x |> invalidParameter pos "'%s' invalid call-with-input-file parameter." |> cont

    let sCallWithOutputFile context pos cont =
        function
        | [ SString f, _; proc ] ->
            let path = f.runes |> runesToString

            try
                let stream = System.IO.File.Create path

                let port =
                    { direction = Output
                      isTextual = true
                      isOpen = true
                      inputReader = None
                      outputWriter = None
                      fileStream = Some stream
                      filePath = Some path }

                proc |> Eval.apply context cont [ SPort port, pos ]
            with :? System.IO.FileNotFoundException as ex ->
                EvalError($"call-with-output-file: {ex.Message}", pos) |> Error |> cont
        | x ->
            x
            |> invalidParameter pos "'%s' invalid call-with-output-file parameter."
            |> cont

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

    let sWithInputFromFile context pos cont =
        function
        | [ SString f, _; proc ] ->
            let path = f.runes |> runesToString

            try
                let content = System.IO.File.ReadAllText path

                let port =
                    { direction = Input
                      isTextual = true
                      isOpen = true
                      inputReader = Some(new System.IO.StringReader(content))
                      outputWriter = None
                      fileStream = None
                      filePath = Some path }

                let savedPort = context.ports.input
                context.ports <- { context.ports with input = port }

                let restore cont' result =
                    context.ports <- { context.ports with input = savedPort }
                    closePort port
                    cont' result

                proc |> Eval.apply context (restore cont) []
            with :? System.IO.FileNotFoundException as ex ->
                EvalError($"with-input-from-file: {ex.Message}", pos) |> Error |> cont
        | x -> x |> invalidParameter pos "'%s' invalid with-input-from-file parameter." |> cont

    let sWithOutputToFile context pos cont =
        function
        | [ SString f, _; proc ] ->
            let path = f.runes |> runesToString

            try
                let stream = System.IO.File.Create path

                let port =
                    { direction = Output
                      isTextual = true
                      isOpen = true
                      inputReader = None
                      outputWriter = None
                      fileStream = Some stream
                      filePath = Some path }

                let savedPort = context.ports.output
                context.ports <- { context.ports with output = port }

                let restore cont' result =
                    context.ports <-
                        { context.ports with
                            output = savedPort }

                    closePort port
                    cont' result

                proc |> Eval.apply context (restore cont) []
            with :? System.IO.IOException as ex ->
                EvalError($"with-output-to-file: {ex.Message}", pos) |> Error |> cont
        | x -> x |> invalidParameter pos "'%s' invalid with-output-to-file parameter." |> cont

    let sOpenInputFile context pos cont =
        function
        | [ SString f, _ ] ->
            try
                let path = f.runes |> runesToString
                let stream = System.IO.File.OpenRead path

                let port =
                    { direction = Input
                      isTextual = true
                      isOpen = true
                      inputReader = None
                      outputWriter = None
                      fileStream = Some stream
                      filePath = Some path }

                (SPort port, pos) |> Ok |> cont
            with :? System.IO.FileNotFoundException as ex ->
                EvalError($"open-input-file: {ex.Message}", pos) |> Error |> cont
        | x -> x |> invalidParameter pos "'%s' invalid open-input-file parameter." |> cont

    let sOpenOutputFile context pos cont =
        function
        | [ SString f, _ ] ->
            try
                let path = f.runes |> runesToString
                let stream = System.IO.File.Create path

                let port =
                    { direction = Output
                      isTextual = true
                      isOpen = true
                      inputReader = None
                      outputWriter = None
                      fileStream = Some stream
                      filePath = Some path }

                (SPort port, pos) |> Ok |> cont
            with :? System.IO.FileNotFoundException as ex ->
                EvalError($"open-output-file: {ex.Message}", pos) |> Error |> cont
        | x -> x |> invalidParameter pos "'%s' invalid open-output-file parameter." |> cont

    let sClosePort context pos cont =
        function
        | [ SPort p, _ ] ->
            closePort p
            (SUnspecified, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid close-port parameter." |> cont

    let sCloseInputPort context pos cont =
        function
        | [ SPort p, _ ] ->
            closePort p
            (SUnspecified, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid close-input-port parameter." |> cont

    let sCloseOutputPort context pos cont =
        function
        | [ SPort p, _ ] ->
            closePort p
            (SUnspecified, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid close-output-port parameter." |> cont

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

    let sWrite context pos cont =
        function
        | [ arg ] ->
            arg |> Print.print |> printf "%s"
            (SUnspecified, pos) |> Ok |> cont
        | [ arg; SPort _, _ ] ->
            arg |> Print.print |> printf "%s"
            (SUnspecified, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid write parameter." |> cont

    let getDisplayString =
        function
        | SString x, _ -> x.runes |> runesToString
        | SChar x, _ -> x |> string
        | expr -> expr |> Print.print

    let sDisplay context pos cont =
        function
        | [ arg ] ->
            arg |> getDisplayString |> printf "%s"
            (SUnspecified, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid display parameter." |> cont

    let writeStringToPort p (s: string) =
        match p.outputWriter with
        | Some w -> w.Write s
        | None ->
            match p.fileStream with
            | Some fs when p.isTextual ->
                let bytes = System.Text.Encoding.UTF8.GetBytes s
                fs.Write(bytes, 0, bytes.Length)
            | _ -> ()

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
            writeStringToPort context.ports.output (s.runes |> runesToString)
            (SUnspecified, pos) |> Ok |> cont
        | [ SString s, _; SPort p, _ ] ->
            writeStringToPort p (s.runes |> runesToString)
            (SUnspecified, pos) |> Ok |> cont
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
