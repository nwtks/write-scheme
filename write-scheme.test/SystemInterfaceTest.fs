module WriteScheme.Tests.SystemInterfaceTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext

let newRep () =
    WriteScheme.Repl.newContext [] |> WriteScheme.Repl.rep

[<Fact>]
let ``load`` () =
    let tmp = System.IO.Path.GetTempFileName()

    try
        let rep = newRep ()
        System.IO.File.WriteAllText(tmp, "(define x 42) (+ x 1)")
        $"(load \"{tmp}\")" |> rep |> should haveSubstring "Loaded"
        "(+ x 1)" |> rep |> should equal "43"
    finally
        System.IO.File.Delete tmp

    try
        let rep = newRep ()
        System.IO.File.WriteAllText(tmp, "(define y 99)")
        let envExpr = "(environment (scheme base))"
        $"(load \"{tmp}\" {envExpr})" |> rep |> should haveSubstring "Loaded"
    finally
        System.IO.File.Delete tmp

    try
        let rep = newRep ()
        System.IO.File.Delete tmp
        $"(load \"{tmp}\")" |> rep |> should startWith "File not found:"
    finally
        ()

    "(load \"test.scm\" \"not-an-environment\")"
    |> rep
    |> should startWith "'(\"test.scm\" \"not-an-environment\")' invalid load parameter."

[<Fact>]
let ``file-exists?`` () =
    let tmp = System.IO.Path.GetTempFileName()

    try
        let rep = newRep ()
        $"(file-exists? \"{tmp}\")" |> rep |> should equal "#t"
    finally
        System.IO.File.Delete tmp

    "(file-exists? \"/nonexistent-file-that-does-not-exist-xyz\")"
    |> rep
    |> should equal "#f"

    "(file-exists? 1)"
    |> rep
    |> should startWith "'(1)' invalid file-exists? parameter"

    "(file-exists?)"
    |> rep
    |> should startWith "'()' invalid file-exists? parameter"

    "(file-exists? \"a\" \"b\")"
    |> rep
    |> should startWith "'(\"a\" \"b\")' invalid file-exists? parameter"

[<Fact>]
let ``delete-file`` () =
    let tmp = System.IO.Path.GetTempFileName()

    try
        let rep = newRep ()
        $"(delete-file \"{tmp}\")" |> rep |> should equal "#<unspecified>"
        System.IO.File.Exists tmp |> should equal false
    finally
        if System.IO.File.Exists tmp then
            System.IO.File.Delete tmp

    "(delete-file \"/nonexistent-file-that-does-not-exist-xyz\")"
    |> rep
    |> should haveSubstring "delete-file: file not found"

    "(delete-file 1)"
    |> rep
    |> should startWith "'(1)' invalid delete-file parameter"

    "(delete-file)" |> rep |> should startWith "'()' invalid delete-file parameter"

    "(delete-file \"a\" \"b\")"
    |> rep
    |> should startWith "'(\"a\" \"b\")' invalid delete-file parameter"

[<Fact>]
let ``command-line`` () =
    let repNoArgs = newRep ()
    "(command-line)" |> repNoArgs |> should equal "()"

    let repWithArgs =
        WriteScheme.Repl.newContext [ "scheme"; "--help"; "input.scm" ]
        |> WriteScheme.Repl.rep

    "(command-line)"
    |> repWithArgs
    |> should equal "(\"scheme\" \"--help\" \"input.scm\")"

    "(begin (import (scheme process-context)) (command-line))"
    |> rep
    |> should equal "()"

    "(command-line \"extra\")"
    |> rep
    |> should startWith "'(\"extra\")' invalid command-line parameter."

[<Fact>]
let ``exit`` () =
    "(procedure? exit)" |> rep |> should equal "#t"

    "(begin (import (scheme process-context)) (procedure? exit))"
    |> rep
    |> should equal "#t"

    "(exit 0 1)" |> rep |> should startWith "'(0 1)' invalid exit parameter."
    "(exit 0 1 2)" |> rep |> should startWith "'(0 1 2)' invalid exit parameter."

    "(exit \"invalid\")"
    |> rep
    |> should startWith "'(\"invalid\")' invalid exit parameter."

    "(exit 'symbol)" |> rep |> should startWith "'(symbol)' invalid exit parameter."

[<Fact>]
let ``emergency-exit`` () =
    "(procedure? emergency-exit)" |> rep |> should equal "#t"

    "(emergency-exit 0 1)"
    |> rep
    |> should startWith "'(0 1)' invalid emergency-exit parameter."

    "(emergency-exit 0 1 2)"
    |> rep
    |> should startWith "'(0 1 2)' invalid emergency-exit parameter."

    "(emergency-exit \"invalid\")"
    |> rep
    |> should startWith "'(\"invalid\")' invalid emergency-exit parameter."

    "(emergency-exit 'symbol)"
    |> rep
    |> should startWith "'(symbol)' invalid emergency-exit parameter."

[<Fact>]
let ``get-environment-variable`` () =
    "(get-environment-variable \"PATH\")" |> rep |> should startWith "\""

    "(get-environment-variable \"__THIS_VAR_DOES_NOT_EXIST_12345__\")"
    |> rep
    |> should equal "#f"

    "(get-environment-variable)"
    |> rep
    |> should startWith "'()' invalid get-environment-variable parameter."

    "(get-environment-variable \"a\" \"b\")"
    |> rep
    |> should startWith "'(\"a\" \"b\")' invalid get-environment-variable parameter."

    "(get-environment-variable 123)"
    |> rep
    |> should startWith "'(123)' invalid get-environment-variable parameter."

[<Fact>]
let ``get-environment-variables`` () =
    "(get-environment-variables)" |> rep |> should startWith "("
    "(get-environment-variables)" |> rep |> should haveSubstring "PATH"

    "(map (lambda (e) (pair? e)) (get-environment-variables))"
    |> rep
    |> should not' (equal "#f")

    "(begin (import (scheme process-context)) (get-environment-variables))"
    |> rep
    |> should startWith "("

    "(get-environment-variables \"extra\")"
    |> rep
    |> should startWith "'(\"extra\")' invalid get-environment-variables parameter."

[<Fact>]
let ``current-second`` () =
    "(current-second)"
    |> rep
    |> float
    |> should be (inRange 1_700_000_000.0 2_000_000_000.0)

    "(begin (import (scheme time)) (current-second))"
    |> rep
    |> float
    |> should be (greaterThan 1_700_000_000.0)

    "(current-second \"extra\")"
    |> rep
    |> should startWith "'(\"extra\")' invalid current-second parameter."

[<Fact>]
let ``current-jiffy`` () =
    let intVal = "(current-jiffy)" |> rep |> bigint.Parse
    intVal |> should be (greaterThan 0I)

    "(current-jiffy)"
    |> rep
    |> bigint.Parse
    |> should be (greaterThanOrEqualTo intVal)

    "(begin (import (scheme time)) (current-jiffy))"
    |> rep
    |> bigint.Parse
    |> should be (greaterThan 0I)

    "(current-jiffy \"extra\")"
    |> rep
    |> should startWith "'(\"extra\")' invalid current-jiffy parameter."

[<Fact>]
let ``jiffies-per-second`` () =
    "(jiffies-per-second)" |> rep |> bigint.Parse |> should be (greaterThan 0I)

    "(begin (import (scheme time)) (jiffies-per-second))"
    |> rep
    |> bigint.Parse
    |> should be (greaterThan 0I)

    "(jiffies-per-second \"extra\")"
    |> rep
    |> should startWith "'(\"extra\")' invalid jiffies-per-second parameter."

[<Fact>]
let ``features`` () =
    let result = "(features)" |> rep
    result |> should startWith "("
    result |> should haveSubstring "r7rs"
    result |> should haveSubstring "exact-closed"
    result |> should haveSubstring "exact-rational"
    result |> should haveSubstring "ieee-float"
    result |> should haveSubstring "full-unicode"
    result |> should haveSubstring "ratios"

    "(features \"extra\")"
    |> rep
    |> should startWith "'(\"extra\")' invalid features parameter."
