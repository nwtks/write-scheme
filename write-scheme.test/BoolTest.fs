module WriteScheme.Tests.BoolTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtin

[<Fact>]
let ``not`` () =
    "(not #t)" |> rep |> should equal "#f"
    "(not 3)" |> rep |> should equal "#f"
    "(not (list 3))" |> rep |> should equal "#f"
    "(not #f)" |> rep |> should equal "#t"
    "(not '())" |> rep |> should equal "#f"
    "(not (list))" |> rep |> should equal "#f"
    "(not 'nil)" |> rep |> should equal "#f"

[<Fact>]
let ``boolean?`` () =
    "(boolean? #f)" |> rep |> should equal "#t"
    "(boolean? #t)" |> rep |> should equal "#t"
    "(boolean? 0)" |> rep |> should equal "#f"
    "(boolean? '())" |> rep |> should equal "#f"

[<Fact>]
let ``boolean=?`` () =
    "(boolean=? #t #t)" |> rep |> should equal "#t"
    "(boolean=? #f #f)" |> rep |> should equal "#t"
    "(boolean=? #t #f)" |> rep |> should equal "#f"
    "(boolean=? #f #t)" |> rep |> should equal "#f"
    "(boolean=? #t #t #t)" |> rep |> should equal "#t"
    "(boolean=? #t #t #f)" |> rep |> should equal "#f"
