module WriteScheme.Tests.InclusionTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext

[<Fact>]
let ``include error paths`` () =
    "(include 1)" |> rep |> should startWith "'(1)' invalid include parameter"
