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
let ``call-with-input-file`` () =
    let tmp = System.IO.Path.GetTempFileName()

    try
        System.IO.File.WriteAllText(tmp, "hello world")

        $"(call-with-input-file \"{tmp}\" (lambda (p) (read-char p)))"
        |> rep
        |> should equal "#\\h"

        $"(call-with-input-file \"{tmp}\" (lambda (p) (read-char p) (input-port-open? p)))"
        |> rep
        |> should equal "#t"
    finally
        System.IO.File.Delete tmp

    try
        System.IO.File.Delete tmp

        $"(call-with-input-file \"{tmp}\" (lambda (p) (read-char p)))"
        |> rep
        |> should startWith "call-with-input-file: Could not find file"
    finally
        ()

    "(call-with-input-file 123 (lambda (p) #f))"
    |> rep
    |> should startWith "'(123 #<procedure>)' invalid call-with-input-file parameter"

[<Fact>]
let ``call-with-output-file`` () =
    let tmp = System.IO.Path.GetTempFileName()

    try
        $"(call-with-output-file \"{tmp}\" (lambda (p) (write-char #\\a p)))"
        |> rep
        |> should equal "#<unspecified>"

        System.IO.File.ReadAllText tmp |> should equal "a"

        $"(call-with-output-file \"{tmp}\" (lambda (p) (write-string \"hi\" p)))"
        |> rep
        |> should equal "#<unspecified>"

        System.IO.File.ReadAllText tmp |> should equal "hi"
    finally
        System.IO.File.Delete tmp

    "(call-with-output-file 1 (lambda (p) #f))"
    |> rep
    |> should startWith "'(1 #<procedure>)' invalid call-with-output-file"

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
    "(input-port-open? (current-output-port))" |> rep |> should equal "#f"
    "(input-port-open? (open-input-bytevector #u8(65)))" |> rep |> should equal "#t"

    "(let ((p (open-input-string \"a\"))) (close-port p) (input-port-open? p))"
    |> rep
    |> should equal "#f"

[<Fact>]
let ``output-port-open?`` () =
    "(output-port-open? (current-output-port))" |> rep |> should equal "#t"
    "(output-port-open? (current-input-port))" |> rep |> should equal "#f"

    "(let ((p (open-output-string))) (close-port p) (output-port-open? p))"
    |> rep
    |> should equal "#f"

[<Fact>]
let ``current-input-port`` () =
    "(current-input-port 1)"
    |> rep
    |> should startWith "'(1)' invalid current-input-port parameter"

[<Fact>]
let ``current-output-port`` () =
    "(current-output-port 1)"
    |> rep
    |> should startWith "'(1)' invalid current-output-port parameter"

[<Fact>]
let ``current-error-port`` () =
    "(current-error-port 1)"
    |> rep
    |> should startWith "'(1)' invalid current-error-port parameter"

[<Fact>]
let ``with-input-from-file`` () =
    let tmp = System.IO.Path.GetTempFileName()

    try
        System.IO.File.WriteAllText(tmp, "hello world")

        $"(with-input-from-file \"{tmp}\" (lambda () (read-char)))"
        |> rep
        |> should equal "#\\h"
    finally
        System.IO.File.Delete tmp

    try
        System.IO.File.WriteAllText(tmp, "hello world")

        $"(with-input-from-file \"{tmp}\" (lambda () (read-line)))"
        |> rep
        |> should equal "\"hello world\""
    finally
        System.IO.File.Delete tmp

    try
        System.IO.File.WriteAllText(tmp, "hello")

        $"(begin (with-input-from-file \"{tmp}\" (lambda () (read-char))) (input-port? (current-input-port)))"
        |> rep
        |> should equal "#t"
    finally
        System.IO.File.Delete tmp

    try
        System.IO.File.Delete tmp

        $"(with-input-from-file \"{tmp}\" (lambda () #f))"
        |> rep
        |> should startWith "with-input-from-file: Could not find file"
    finally
        ()

    "(with-input-from-file 123 (lambda () #f))"
    |> rep
    |> should startWith "'(123 #<procedure>)' invalid with-input-from-file parameter"

[<Fact>]
let ``with-output-to-file`` () =
    let tmp = System.IO.Path.GetTempFileName()

    try
        $"(with-output-to-file \"{tmp}\" (lambda () (write-string \"hello\")))"
        |> rep
        |> should equal "#<unspecified>"

        System.IO.File.ReadAllText tmp |> should equal "hello"
    finally
        System.IO.File.Delete tmp

    try
        $"(with-output-to-file \"{tmp}\" (lambda () (write-string \"world\")))"
        |> rep
        |> should equal "#<unspecified>"

        System.IO.File.ReadAllText tmp |> should equal "world"
    finally
        System.IO.File.Delete tmp

    try
        $"(begin (with-output-to-file \"{tmp}\" (lambda () (write-string \"hi\"))) (output-port? (current-output-port)))"
        |> rep
        |> should equal "#t"
    finally
        System.IO.File.Delete tmp

    "(with-output-to-file 123 (lambda () #f))"
    |> rep
    |> should startWith "'(123 #<procedure>)' invalid with-output-to-file parameter"

[<Fact>]
let ``open-input-file`` () =
    let tmp = System.IO.Path.GetTempFileName()

    try
        System.IO.File.WriteAllText(tmp, "hello world")

        $"(let ((p (open-input-file \"{tmp}\"))) (read-char p))"
        |> rep
        |> should equal "#\\h"

        $"(let ((p (open-input-file \"{tmp}\"))) (read-char p) (read-char p) (read-char p))"
        |> rep
        |> should equal "#\\l"
    finally
        System.IO.File.Delete tmp

    try
        System.IO.File.Delete tmp

        $"(open-input-file \"{tmp}\")"
        |> rep
        |> should startWith "open-input-file: Could not find file"
    finally
        ()

    "(open-input-file 123)"
    |> rep
    |> should startWith "'(123)' invalid open-input-file parameter"

[<Fact>]
let ``open-binary-input-file`` () =
    let tmp = System.IO.Path.GetTempFileName()

    try
        System.IO.File.WriteAllBytes(tmp, [| 0x48uy; 0x65uy; 0x6Cuy; 0x6Cuy; 0x6Fuy |])

        $"(let ((p (open-binary-input-file \"{tmp}\"))) (read-u8 p))"
        |> rep
        |> should equal "72"
    finally
        System.IO.File.Delete tmp

    try
        System.IO.File.WriteAllBytes(tmp, [| 0x00uy |])

        $"(let ((p (open-binary-input-file \"{tmp}\"))) (binary-port? p))"
        |> rep
        |> should equal "#t"
    finally
        System.IO.File.Delete tmp

    try
        System.IO.File.WriteAllBytes(tmp, [| 0x41uy; 0x42uy; 0x43uy |])

        $"(let ((p (open-binary-input-file \"{tmp}\"))) (read-u8 p) (read-u8 p) (read-u8 p))"
        |> rep
        |> should equal "67"
    finally
        System.IO.File.Delete tmp

    try
        System.IO.File.WriteAllBytes(tmp, [| 0x41uy; 0x42uy; 0x43uy; 0x44uy |])

        $"(let ((p (open-binary-input-file \"{tmp}\"))) (read-bytevector 3 p))"
        |> rep
        |> should equal "#u8(65 66 67)"
    finally
        System.IO.File.Delete tmp

    try
        System.IO.File.WriteAllBytes(tmp, [| 0x41uy |])

        $"(let ((p (open-binary-input-file \"{tmp}\"))) (read-u8 p) (read-u8 p))"
        |> rep
        |> should equal "#!eof"
    finally
        System.IO.File.Delete tmp

    try
        System.IO.File.WriteAllBytes(tmp, [| 0x41uy |])

        $"(let ((p (open-binary-input-file \"{tmp}\"))) (close-port p) (input-port-open? p))"
        |> rep
        |> should equal "#f"
    finally
        System.IO.File.Delete tmp

    try
        System.IO.File.Delete tmp

        $"(open-binary-input-file \"{tmp}\")"
        |> rep
        |> should startWith "open-binary-input-file: Could not find file"
    finally
        ()

    "(open-binary-input-file 123)"
    |> rep
    |> should startWith "'(123)' invalid open-binary-input-file parameter"

[<Fact>]
let ``open-output-file`` () =
    "(open-output-file 123)"
    |> rep
    |> should startWith "'(123)' invalid open-output-file parameter"

[<Fact>]
let ``open-binary-output-file`` () =
    let tmp = System.IO.Path.GetTempFileName()

    try
        $"(let ((p (open-binary-output-file \"{tmp}\"))) (write-u8 65 p) (close-port p))"
        |> rep
        |> should equal "#<unspecified>"

        System.IO.File.ReadAllBytes tmp |> should equal [| 65uy |]
    finally
        System.IO.File.Delete tmp

    try
        $"(binary-port? (open-binary-output-file \"{tmp}\"))"
        |> rep
        |> should equal "#t"
    finally
        System.IO.File.Delete tmp

    try
        $"(let ((p (open-binary-output-file \"{tmp}\"))) (write-bytevector #u8(65 66 67) p) (close-port p))"
        |> rep
        |> should equal "#<unspecified>"

        System.IO.File.ReadAllBytes tmp |> should equal [| 65uy; 66uy; 67uy |]
    finally
        System.IO.File.Delete tmp

    try
        $"(let ((p (open-binary-output-file \"{tmp}\"))) (close-port p) (output-port-open? p))"
        |> rep
        |> should equal "#f"
    finally
        System.IO.File.Delete tmp

    "(open-binary-output-file 123)"
    |> rep
    |> should startWith "'(123)' invalid open-binary-output-file parameter"

[<Fact>]
let ``close-port`` () =
    "(let ((p (open-input-string \"a\"))) (close-port p) (input-port-open? p))"
    |> rep
    |> should equal "#f"

    "(close-port 1)" |> rep |> should startWith "'(1)' invalid close-port parameter"
    "(close-port)" |> rep |> should startWith "'()' invalid close-port parameter"

[<Fact>]
let ``close-input-port`` () =
    "(close-input-port 1)"
    |> rep
    |> should startWith "'(1)' invalid close-input-port parameter"

[<Fact>]
let ``close-output-port`` () =
    "(close-output-port 1)"
    |> rep
    |> should startWith "'(1)' invalid close-output-port parameter"

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

    "(open-output-bytevector 1)"
    |> rep
    |> should startWith "'(1)' invalid open-output-bytevector parameter"

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
let ``read`` () =
    "(read (open-input-string \"42\"))" |> rep |> should equal "42"
    "(read (open-input-string \"hello\"))" |> rep |> should equal "hello"
    "(read (open-input-string \"(a b c)\"))" |> rep |> should equal "(a b c)"
    "(read (open-input-bytevector #u8(65)))" |> rep |> should equal "#!eof"

    "(read (open-input-string \")\"))" |> rep |> should startWith "Error"

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

    "(read-char (open-input-bytevector #u8(65)))" |> rep |> should equal "#\\A"
    "(read-char (open-input-bytevector #u8()))" |> rep |> should equal "#!eof"

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

    "(peek-char (open-input-bytevector #u8(65)))" |> rep |> should equal "#\\A"
    "(peek-char (open-input-bytevector #u8()))" |> rep |> should equal "#!eof"

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

    "(eof-object 1)" |> rep |> should startWith "'(1)' invalid eof-object parameter"

[<Fact>]
let ``char-ready?`` () =
    "(char-ready?)" |> rep |> should equal "#t"
    "(char-ready? (open-input-string \"a\"))" |> rep |> should equal "#t"

    "(char-ready? 1)"
    |> rep
    |> should startWith "'(1)' invalid char-ready? parameter"

[<Fact>]
let ``read-string`` () =
    "(read-string 5 (open-input-string \"hello\"))"
    |> rep
    |> should equal "\"hello\""

    "(read-string 3 (open-input-string \"hi\"))" |> rep |> should equal "\"hi\""

    "(read-string \"abc\")"
    |> rep
    |> should startWith "'(\"abc\")' invalid read-string parameter"

    "(read-string -1 (open-input-string \"abc\"))"
    |> rep
    |> should startWith "'(-1 #<input textual port open>)' invalid read-string parameter"

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
let ``peek-u8`` () =
    "(let ((p (open-input-bytevector #u8(65 66 67)))) (peek-u8 p))"
    |> rep
    |> should equal "65"

    "(let ((p (open-input-bytevector #u8(65 66 67)))) (peek-u8 p) (peek-u8 p))"
    |> rep
    |> should equal "65"

    "(let ((p (open-input-bytevector #u8(65 66 67)))) (peek-u8 p) (read-u8 p) (peek-u8 p))"
    |> rep
    |> should equal "66"

    "(let ((p (open-input-bytevector #u8()))) (peek-u8 p))"
    |> rep
    |> should equal "#!eof"

    "(peek-u8 1)" |> rep |> should startWith "'(1)' invalid peek-u8 parameter"

[<Fact>]
let ``u8-ready?`` () =
    "(u8-ready?)" |> rep |> should equal "#t"
    "(u8-ready? (open-input-bytevector #u8(65)))" |> rep |> should equal "#t"

    "(u8-ready? 1)" |> rep |> should startWith "'(1)' invalid u8-ready? parameter"

[<Fact>]
let ``read-bytevector`` () =
    "(let ((p (open-input-bytevector #u8(65 66 67)))) (read-bytevector 2 p))"
    |> rep
    |> should equal "#u8(65 66)"

    "(let ((p (open-input-bytevector #u8(65 66 67)))) (read-bytevector 0 p))"
    |> rep
    |> should equal "#!eof"

    "(read-bytevector -1 (open-input-bytevector #u8(65)))"
    |> rep
    |> should startWith "'(-1 #<input binary port open>)' invalid read-bytevector parameter"

    "(read-bytevector \"abc\")"
    |> rep
    |> should startWith "'(\"abc\")' invalid read-bytevector parameter"

[<Fact>]
let ``read-bytevector!`` () =
    "(let ((bv (bytevector 0 0 0)) (p (open-input-bytevector #u8(65 66 67)))) (read-bytevector! bv p) bv)"
    |> rep
    |> should equal "#u8(65 66 67)"

    "(let ((bv (bytevector 0 0)) (p (open-input-bytevector #u8(65 66 67)))) (read-bytevector! bv p) bv)"
    |> rep
    |> should equal "#u8(65 66)"

    "(let ((bv (bytevector 0 0 0)) (p (open-input-bytevector #u8()))) (read-bytevector! bv p))"
    |> rep
    |> should equal "#!eof"

    "(let ((bv (bytevector 0 0 0 0)) (p (open-input-bytevector #u8(65 66 67)))) (read-bytevector! bv p 1) bv)"
    |> rep
    |> should equal "#u8(0 65 66 67)"

    "(let ((bv (bytevector 0 0 0 0 0)) (p (open-input-bytevector #u8(65 66 67 68 69)))) (read-bytevector! bv p 1 4) bv)"
    |> rep
    |> should equal "#u8(0 65 66 67 0)"

    "(let ((bv (bytevector 0 0 0)) (p (open-input-bytevector #u8(65 66 67)))) (read-bytevector! bv p))"
    |> rep
    |> should equal "3"

    "(let ((bv (bytevector 0 1 2))) (read-bytevector! bv (open-input-bytevector #u8(65 66 67)) -1))"
    |> rep
    |> should startWith "read-bytevector!: start index -1 out of range"

    "(let ((bv (bytevector 0 1 2))) (read-bytevector! bv (open-input-bytevector #u8(65 66 67)) 5))"
    |> rep
    |> should startWith "read-bytevector!: start index 5 out of range"

    "(let ((bv (bytevector 0 1 2))) (read-bytevector! bv (open-input-bytevector #u8(65 66 67)) 0 5))"
    |> rep
    |> should startWith "read-bytevector!: end index 5 out of range"

    "(let ((bv (bytevector 0 1 2))) (read-bytevector! bv (open-input-bytevector #u8(65 66 67)) 2 1))"
    |> rep
    |> should startWith "read-bytevector!: start index 2 is greater than end index 1"

[<Fact>]
let ``write`` () =
    "(write \"hello\")" |> rep |> should equal "#<unspecified>"

[<Fact>]
let ``write-shared`` () =
    "(let ((p (open-output-string))) (write-shared 42 p) (get-output-string p))"
    |> rep
    |> should equal "\"42\""

    "(let ((p (open-output-string))) (write-shared #t p) (get-output-string p))"
    |> rep
    |> should equal "\"#t\""

    "(let ((p (open-output-string))) (write-shared 'symbol p) (get-output-string p))"
    |> rep
    |> should equal "\"symbol\""

    "(let ((p (open-output-string)) (x (cons 'a 'b))) (write-shared (list x x) p) (get-output-string p))"
    |> rep
    |> should equal "\"(#1=(a . b) #1#)\""

    "(let ((p (open-output-string)) (x (vector 1 2))) (write-shared (vector x x) p) (get-output-string p))"
    |> rep
    |> should equal "\"#(#1=#(1 2) #1#)\""

    "(let ((p (open-output-string))) (write-shared (let ((x (cons 1 2))) (vector x x)) p) (get-output-string p))"
    |> rep
    |> should equal "\"#(#1=(1 . 2) #1#)\""

    "(let ((p (open-output-string))) (write-shared '(a b c) p) (get-output-string p))"
    |> rep
    |> should equal "\"(a b c)\""

    "(let ((p (open-output-string))) (write-shared '#(1 2 3) p) (get-output-string p))"
    |> rep
    |> should equal "\"#(1 2 3)\""

    "(write-shared)"
    |> rep
    |> should startWith "'()' invalid write-shared parameter"

    "(write-shared 1 2 3)"
    |> rep
    |> should startWith "'(1 2 3)' invalid write-shared parameter"

[<Fact>]
let ``write-simple`` () =
    "(let ((p (open-output-string))) (write-simple 42 p) (get-output-string p))"
    |> rep
    |> should equal "\"42\""

    "(let ((p (open-output-string))) (write-simple #t p) (get-output-string p))"
    |> rep
    |> should equal "\"#t\""

    "(let ((p (open-output-string))) (write-simple 'symbol p) (get-output-string p))"
    |> rep
    |> should equal "\"symbol\""

    "(let ((p (open-output-string))) (write-simple '(a b c) p) (get-output-string p))"
    |> rep
    |> should equal "\"(a b c)\""

    "(let ((p (open-output-string))) (write-simple '#(1 2 3) p) (get-output-string p))"
    |> rep
    |> should equal "\"#(1 2 3)\""

    "(write-simple)"
    |> rep
    |> should startWith "'()' invalid write-simple parameter"

    "(write-simple 1 2 3)"
    |> rep
    |> should startWith "'(1 2 3)' invalid write-simple parameter"

[<Fact>]
let ``display`` () =
    "(display \"hello\")" |> rep |> should equal "#<unspecified>"

    "(let ((p (open-output-string))) (display \"hello\" p) (get-output-string p))"
    |> rep
    |> should equal "\"hello\""

    "(let ((p (open-output-string))) (display 42 p) (get-output-string p))"
    |> rep
    |> should equal "\"42\""

    "(let ((p (open-output-string))) (display #\\a p) (get-output-string p))"
    |> rep
    |> should equal "\"a\""

    "(let ((p (open-output-string))) (display #t p) (get-output-string p))"
    |> rep
    |> should equal "\"#t\""

    "(display \"hello\" 1)"
    |> rep
    |> should startWith "'(\"hello\" 1)' invalid display parameter"

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

    "(let ((p (open-output-bytevector))) (write-char #\\a p) (get-output-bytevector p))"
    |> rep
    |> should equal "#u8()"

    "(write-char 123 (open-output-string))"
    |> rep
    |> should startWith "write-char: '123' is not a char"

[<Fact>]
let ``write-string`` () =
    "(let ((p (open-output-string))) (write-string \"hello\" p) (get-output-string p))"
    |> rep
    |> should equal "\"hello\""

    "(let ((p (open-output-string))) (write-string \"\" p) (get-output-string p))"
    |> rep
    |> should equal "\"\""

    "(let ((p (open-output-string))) (write-string \"hello\" p 1) (get-output-string p))"
    |> rep
    |> should equal "\"ello\""

    "(let ((p (open-output-string))) (write-string \"hello\" p 1 4) (get-output-string p))"
    |> rep
    |> should equal "\"ell\""

    "(let ((p (open-output-string))) (write-string \"hello\" p 0 5) (get-output-string p))"
    |> rep
    |> should equal "\"hello\""

    "(write-string \"hello\")" |> rep |> should equal "#<unspecified>"

    "(write-string \"hello\" 1)"
    |> rep
    |> should startWith "'(\"hello\" 1)' invalid write-string parameter"

[<Fact>]
let ``write-u8`` () =
    "(let ((p (open-output-bytevector))) (write-u8 65 p) (get-output-bytevector p))"
    |> rep
    |> should equal "#u8(65)"

    "(write-u8 300 (open-output-bytevector))"
    |> rep
    |> should startWith "'(300 #<output binary port open>)' invalid write-u8 parameter"

    "(write-u8 65 (open-output-string))"
    |> rep
    |> should startWith "'(65 #<output textual port open>)' invalid write-u8 parameter"

[<Fact>]
let ``write-bytevector`` () =
    "(let ((p (open-output-bytevector))) (write-bytevector #u8(1 2 3) p) (get-output-bytevector p))"
    |> rep
    |> should equal "#u8(1 2 3)"

    "(write-bytevector #u8(1 2 3) (current-output-port))"
    |> rep
    |> should startWith "'(#u8(1 2 3) #<output textual port open>)' invalid write-bytevector parameter"

[<Fact>]
let ``flush-output-port`` () =
    "(flush-output-port (current-output-port))"
    |> rep
    |> should equal "#<unspecified>"

    "(let ((p (open-output-string))) (write-char #\\a p) (flush-output-port p) (get-output-string p))"
    |> rep
    |> should equal "\"a\""

    "(flush-output-port (current-input-port))"
    |> rep
    |> should equal "#<unspecified>"
