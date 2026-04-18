module WriteScheme.Tests.SymbolTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtin

[<Fact>]
let ``symbol?`` () =
    "(symbol? 'foo)" |> rep |> should equal "#t"
    "(symbol? (car '(a b)))" |> rep |> should equal "#t"
    "(symbol? \"bar\")" |> rep |> should equal "#f"
    "(symbol? 'nil)" |> rep |> should equal "#t"
    "(symbol? '())" |> rep |> should equal "#f"
    "(symbol? #f)" |> rep |> should equal "#f"

[<Fact>]
let ``symbol=?`` () =
    "(symbol=? 'abc 'abc)" |> rep |> should equal "#t"
    "(symbol=? 'abc 'def)" |> rep |> should equal "#f"
    "(symbol=? 'abc 'abc 'abc)" |> rep |> should equal "#t"
    "(symbol=? 'abc 'abc 'def)" |> rep |> should equal "#f"
    "(symbol=? 'foo)" |> rep |> should equal "#t"

[<Fact>]
let ``symbol->string`` () =
    "(symbol->string 'abc)" |> rep |> should equal "\"abc\""
    "(symbol->string 'hello-world)" |> rep |> should equal "\"hello-world\""

[<Fact>]
let ``string->symbol`` () =
    "(string->symbol \"abc\")" |> rep |> should equal "abc"
    "(string->symbol \"hello-world\")" |> rep |> should equal "hello-world"
