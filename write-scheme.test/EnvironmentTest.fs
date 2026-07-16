module WriteScheme.Tests.EnvironmentTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext

[<Fact>]
let ``environment`` () =
    "(environment)" |> rep |> should equal "#<environment>"
    "(environment (scheme base))" |> rep |> should equal "#<environment>"

    "(environment (scheme base) (scheme eval))"
    |> rep
    |> should equal "#<environment>"

[<Fact>]
let ``scheme-report-environment`` () =
    "(scheme-report-environment 5)" |> rep |> should equal "#<environment>"
    "(eval '(+ 1 2) (scheme-report-environment 5))" |> rep |> should equal "3"

    "(eval 'car (scheme-report-environment 5))"
    |> rep
    |> should equal "#<procedure>"

    "(scheme-report-environment)"
    |> rep
    |> should startWith "scheme-report-environment: missing argument."

    "(scheme-report-environment 0)"
    |> rep
    |> should startWith "scheme-report-environment: only version 5 is supported"

    "(scheme-report-environment 'a)"
    |> rep
    |> should startWith "scheme-report-environment: argument must be an exact integer"

[<Fact>]
let ``null-environment`` () =
    "(null-environment 5)" |> rep |> should equal "#<environment>"

    "(eval '(+ 1 2) (null-environment 5))"
    |> rep
    |> should startWith "No binding for '+'"

    "(null-environment)"
    |> rep
    |> should startWith "null-environment: missing argument."

    "(null-environment 0)"
    |> rep
    |> should startWith "null-environment: only version 5 is supported"

    "(null-environment 'a)"
    |> rep
    |> should startWith "null-environment: argument must be an exact integer"

[<Fact>]
let ``interaction-environment`` () =
    "(interaction-environment)" |> rep |> should equal "#<environment>"
    "(eval '(+ 1 2) (interaction-environment))" |> rep |> should equal "3"

    "(interaction-environment 1)"
    |> rep
    |> should startWith "'(1)' invalid interaction-environment parameter"

[<Fact>]
let ``eval basic expression`` () =
    "(eval '(+ 1 2) (environment (scheme base)))" |> rep |> should equal "3"
    "(eval 'car (environment (scheme base)))" |> rep |> should equal "#<procedure>"
    "(eval ''(1 2 3) (environment (scheme base)))" |> rep |> should equal "(1 2 3)"

    "(eval '((lambda (x) (* x x)) 5) (environment (scheme base)))"
    |> rep
    |> should equal "25"

    "(eval '(+ 1 2) (environment))" |> rep |> should startWith "No binding for '+"

    "(eval '(+ 1 2))"
    |> rep
    |> should startWith "eval: missing environment argument."

    "(eval '(+ 1 2) 'not-an-environment)"
    |> rep
    |> should startWith "eval: second argument must be an environment object"
