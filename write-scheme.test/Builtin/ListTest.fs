module WriteScheme.Tests.ListTest

open Xunit
open FsUnit.Xunit
open WriteScheme.Builtin
open WriteScheme.Repl

[<Fact>]
let ``pair?`` () =
    "(pair? '(a . b))" |> rep builtin |> should equal "#t"
    "(pair? '(a b c))" |> rep builtin |> should equal "#t"
    "(pair? '())" |> rep builtin |> should equal "#f"

[<Fact>]
let cons () =
    "(cons 'a '())" |> rep builtin |> should equal "(a)"
    "(cons '(a) '(b c d))" |> rep builtin |> should equal "((a) b c d)"
    "(cons \"a\" '(b c))" |> rep builtin |> should equal "(\"a\" b c)"
    "(cons 'a 3)" |> rep builtin |> should equal "(a . 3)"
    "(cons '(a b) 'c)" |> rep builtin |> should equal "((a b) . c)"

[<Fact>]
let car () =
    "(car '(a b c))" |> rep builtin |> should equal "a"
    "(car '((a) b c d))" |> rep builtin |> should equal "(a)"
    "(car '(1 . 2))" |> rep builtin |> should equal "1"
    "(car '(1 2 . 3))" |> rep builtin |> should equal "1"

[<Fact>]
let cdr () =
    "(cdr '(a b c))" |> rep builtin |> should equal "(b c)"
    "(cdr '((a) b c d))" |> rep builtin |> should equal "(b c d)"
    "(cdr '(1 . 2))" |> rep builtin |> should equal "2"
    "(cdr '(1 2 . 3))" |> rep builtin |> should equal "(2 . 3)"

[<Fact>]
let ``null?`` () =
    "(null? '(a . b))" |> rep builtin |> should equal "#f"
    "(null? '(a b c))" |> rep builtin |> should equal "#f"
    "(null? '())" |> rep builtin |> should equal "#t"

[<Fact>]
let ``list?`` () =
    "(list? '(a . b))" |> rep builtin |> should equal "#f"
    "(list? '(a b c))" |> rep builtin |> should equal "#t"
    "(list? '())" |> rep builtin |> should equal "#t"

[<Fact>]
let list () =
    "(list 'a (+ 3 4) 'c)" |> rep builtin |> should equal "(a 7 c)"
    "(list)" |> rep builtin |> should equal "()"

[<Fact>]
let append () =
    "(append '(x) '(y))" |> rep builtin |> should equal "(x y)"
    "(append '(a) '(b c d))" |> rep builtin |> should equal "(a b c d)"
    "(append '(a (b)) '((c)))" |> rep builtin |> should equal "(a (b) (c))"
    "(append '(a b) '(c . d))" |> rep builtin |> should equal "(a b c . d)"
    "(append '() 'a)" |> rep builtin |> should equal "a"
