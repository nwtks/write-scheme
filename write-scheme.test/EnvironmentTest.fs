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

    "(begin (import (scheme eval)) (environment))"
    |> rep
    |> should equal "#<environment>"

    "(environment (nonexistent library))"
    |> rep
    |> should startWith "Library '(nonexistent library)' not found."
