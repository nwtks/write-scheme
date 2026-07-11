module WriteScheme.Tests.PortTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext

[<Fact>]
let ``call-with-port`` () =
    "(call-with-port (open-input-string \"hello\") (lambda (p) (read-char p)))"
    |> rep
    |> should equal "#\\h"

    "(call-with-port (open-output-string) (lambda (p) (write-char #\\a p) (get-output-string p)))"
    |> rep
    |> should equal "\"a\""

    "(let ((p (open-input-string \"abc\"))) (call-with-port p (lambda (p2) (read-char p2))) (input-port-open? p))"
    |> rep
    |> should equal "#f"

    "(let ((p (open-input-string \"abc\"))) (guard (ex (else #f)) (call-with-port p (lambda (p2) (error \"oops\")))) (input-port-open? p))"
    |> rep
    |> should equal "#f"

    "(call-with-port 1 (lambda (p) p))"
    |> rep
    |> should startWith "call-with-port: '1' is not a port."

    "(call-with-port (current-input-port))"
    |> rep
    |> should startWith "'(#<input textual port open>)' invalid call-with-port parameter"

[<Fact>]
let ``input-port?`` () =
    "(input-port? (current-input-port))" |> rep |> should equal "#t"
    "(input-port? (current-output-port))" |> rep |> should equal "#f"

    "(input-port? 1)"
    |> rep
    |> should startWith "'(1)' invalid input-port? parameter"

[<Fact>]
let ``output-port?`` () =
    "(output-port? (current-output-port))" |> rep |> should equal "#t"
    "(output-port? (current-input-port))" |> rep |> should equal "#f"

    "(output-port? 1)"
    |> rep
    |> should startWith "'(1)' invalid output-port? parameter"

[<Fact>]
let ``textual-port?`` () =
    "(textual-port? (current-input-port))" |> rep |> should equal "#t"

    "(textual-port? 1)"
    |> rep
    |> should startWith "'(1)' invalid textual-port? parameter"

[<Fact>]
let ``binary-port?`` () =
    "(binary-port? (current-input-port))" |> rep |> should equal "#f"

    "(binary-port? 1)"
    |> rep
    |> should startWith "'(1)' invalid binary-port? parameter"

[<Fact>]
let ``port?`` () =
    "(port? (current-input-port))" |> rep |> should equal "#t"
    "(port? (current-output-port))" |> rep |> should equal "#t"
    "(port? (current-error-port))" |> rep |> should equal "#t"
    "(port? '())" |> rep |> should equal "#f"
    "(port? 1)" |> rep |> should equal "#f"
    "(port? 1 2)" |> rep |> should startWith "'(1 2)' invalid port? parameter"

[<Fact>]
let ``input-port-open?`` () =
    "(input-port-open? (current-input-port))" |> rep |> should equal "#t"

[<Fact>]
let ``output-port-open?`` () =
    "(output-port-open? (current-output-port))" |> rep |> should equal "#t"

[<Fact>]
let ``with-input-from-file`` () =
    let tmp = System.IO.Path.GetTempFileName()

    try
        System.IO.File.WriteAllText(tmp, "hello world")
        let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext
        let result = $"(with-input-from-file \"{tmp}\" (lambda () (read-char)))" |> rep
        result |> should equal "#\\h"
    finally
        System.IO.File.Delete tmp

    try
        System.IO.File.WriteAllText(tmp, "hello world")
        let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext
        let result = $"(with-input-from-file \"{tmp}\" (lambda () (read-line)))" |> rep
        result |> should equal "\"hello world\""
    finally
        System.IO.File.Delete tmp

    try
        System.IO.File.WriteAllText(tmp, "hello")
        let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext

        let result =
            $"(begin (with-input-from-file \"{tmp}\" (lambda () (read-char))) (input-port? (current-input-port)))"
            |> rep

        result |> should equal "#t"
    finally
        System.IO.File.Delete tmp

    try
        System.IO.File.Delete tmp

        $"(with-input-from-file \"{tmp}\" (lambda () #f))"
        |> rep
        |> should startWith "with-input-from-file: Could not find file"
    finally
        ()

[<Fact>]
let ``with-output-to-file`` () =
    let tmp = System.IO.Path.GetTempFileName()

    try
        let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext

        $"(with-output-to-file \"{tmp}\" (lambda () (write-string \"hello\")))"
        |> rep
        |> should equal "#<unspecified>"

        System.IO.File.ReadAllText tmp |> should equal "hello"
    finally
        System.IO.File.Delete tmp

    try
        let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext

        $"(with-output-to-file \"{tmp}\" (lambda () (write-string \"world\")))"
        |> rep
        |> should equal "#<unspecified>"

        System.IO.File.ReadAllText tmp |> should equal "world"
    finally
        System.IO.File.Delete tmp

    try
        let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext

        $"(begin (with-output-to-file \"{tmp}\" (lambda () (write-string \"hi\"))) (output-port? (current-output-port)))"
        |> rep
        |> should equal "#t"
    finally
        System.IO.File.Delete tmp

[<Fact>]
let ``close-port`` () =
    "(let ((p (open-input-string \"a\"))) (close-port p) (input-port-open? p))"
    |> rep
    |> should equal "#f"

[<Fact>]
let ``open-input-string`` () =
    "(open-input-string \"hello\")"
    |> rep
    |> should startWith "#<input textual port open>"

    "(open-input-string 1)"
    |> rep
    |> should startWith "'(1)' invalid open-input-string parameter"

[<Fact>]
let ``open-output-string`` () =
    "(open-output-string)" |> rep |> should startWith "#<output textual port open>"

    "(open-output-string 1)"
    |> rep
    |> should startWith "'(1)' invalid open-output-string parameter"

[<Fact>]
let ``get-output-string`` () =
    "(let ((p (open-output-string))) (write-char #\\a p) (get-output-string p))"
    |> rep
    |> should equal "\"a\""

    "(let ((p (open-output-string))) (write-char #\\a p) (write-char #\\b p) (get-output-string p))"
    |> rep
    |> should equal "\"ab\""

    "(let ((p (open-output-string))) (write-string \"hello\" p) (get-output-string p))"
    |> rep
    |> should equal "\"hello\""

    "(let ((p (open-output-string))) (get-output-string p))"
    |> rep
    |> should equal "\"\""

    "(get-output-string 1)"
    |> rep
    |> should startWith "'(1)' invalid get-output-string parameter"

[<Fact>]
let ``open-input-bytevector`` () =
    "(open-input-bytevector #u8(1 2 3))"
    |> rep
    |> should startWith "#<input binary port open>"

    "(open-input-bytevector 1)"
    |> rep
    |> should startWith "'(1)' invalid open-input-bytevector parameter"

[<Fact>]
let ``open-output-bytevector`` () =
    "(open-output-bytevector)"
    |> rep
    |> should startWith "#<output binary port open>"

[<Fact>]
let ``get-output-bytevector`` () =
    "(let ((p (open-output-bytevector))) (write-u8 65 p) (get-output-bytevector p))"
    |> rep
    |> should equal "#u8(65)"

    "(let ((p (open-output-bytevector))) (write-bytevector #u8(1 2 3) p) (get-output-bytevector p))"
    |> rep
    |> should equal "#u8(1 2 3)"

    "(let ((p (open-output-bytevector))) (get-output-bytevector p))"
    |> rep
    |> should equal "#u8()"

    "(get-output-bytevector 1)"
    |> rep
    |> should startWith "'(1)' invalid get-output-bytevector parameter"

[<Fact>]
let ``read-char`` () =
    "(let ((p (open-input-string \"abc\"))) (read-char p))"
    |> rep
    |> should equal "#\\a"

    "(let ((p (open-input-string \"abc\"))) (read-char p) (read-char p))"
    |> rep
    |> should equal "#\\b"

    "(let ((p (open-input-string \"abc\"))) (read-char p) (read-char p) (read-char p))"
    |> rep
    |> should equal "#\\c"

    "(let ((p (open-input-string \"a\"))) (read-char p) (read-char p))"
    |> rep
    |> should equal "#!eof"

[<Fact>]
let ``peek-char`` () =
    "(let ((p (open-input-string \"abc\"))) (peek-char p))"
    |> rep
    |> should equal "#\\a"

    "(let ((p (open-input-string \"abc\"))) (peek-char p) (read-char p) (peek-char p))"
    |> rep
    |> should equal "#\\b"

    "(let ((p (open-input-string \"\"))) (peek-char p))"
    |> rep
    |> should equal "#!eof"

[<Fact>]
let ``read-line`` () =
    "(let ((p (open-input-string \"hello\"))) (read-line p))"
    |> rep
    |> should equal "\"hello\""

    "(let ((p (open-input-string \"hello\\nworld\"))) (read-line p))"
    |> rep
    |> should equal "\"hello\""

    "(let ((p (open-input-string \"\"))) (read-line p))"
    |> rep
    |> should equal "#!eof"

[<Fact>]
let ``eof-object?`` () =
    "(eof-object? #!eof)" |> rep |> should equal "#t"
    "(eof-object? '())" |> rep |> should equal "#f"
    "(eof-object? 1)" |> rep |> should equal "#f"

    "(eof-object? 1 2)"
    |> rep
    |> should startWith "'(1 2)' invalid eof-object? parameter"

[<Fact>]
let ``eof-object`` () =
    "(eof-object)" |> rep |> should equal "#!eof"

[<Fact>]
let ``read-u8`` () =
    "(let ((p (open-input-bytevector #u8(65 66 67)))) (read-u8 p))"
    |> rep
    |> should equal "65"

    "(let ((p (open-input-bytevector #u8(65 66 67)))) (read-u8 p) (read-u8 p))"
    |> rep
    |> should equal "66"

    "(let ((p (open-input-bytevector #u8()))) (read-u8 p))"
    |> rep
    |> should equal "#!eof"

[<Fact>]
let ``read-bytevector`` () =
    "(let ((p (open-input-bytevector #u8(65 66 67)))) (read-bytevector 2 p))"
    |> rep
    |> should equal "#u8(65 66)"

    "(let ((p (open-input-bytevector #u8(65 66 67)))) (read-bytevector 0 p))"
    |> rep
    |> should equal "#!eof"

[<Fact>]
let ``newline`` () =
    "(let ((p (open-output-string))) (newline p) (string-length (get-output-string p)))"
    |> rep
    |> should equal "1"

[<Fact>]
let ``write-char`` () =
    "(let ((p (open-output-string))) (write-char #\\a p) (get-output-string p))"
    |> rep
    |> should equal "\"a\""

    "(let ((p (open-output-string))) (write-char #\\space p) (get-output-string p))"
    |> rep
    |> should equal "\" \""

[<Fact>]
let ``write-string`` () =
    "(let ((p (open-output-string))) (write-string \"hello\" p) (get-output-string p))"
    |> rep
    |> should equal "\"hello\""

    "(let ((p (open-output-string))) (write-string \"\" p) (get-output-string p))"
    |> rep
    |> should equal "\"\""

[<Fact>]
let ``write-u8`` () =
    "(let ((p (open-output-bytevector))) (write-u8 65 p) (get-output-bytevector p))"
    |> rep
    |> should equal "#u8(65)"

[<Fact>]
let ``write-bytevector`` () =
    "(let ((p (open-output-bytevector))) (write-bytevector #u8(1 2 3) p) (get-output-bytevector p))"
    |> rep
    |> should equal "#u8(1 2 3)"
