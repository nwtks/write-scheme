module WriteScheme.Tests.ListTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtin

[<Fact>]
let ``pair?`` () =
    "(pair? '(a . b))" |> rep |> should equal "#t"
    "(pair? '(a b c))" |> rep |> should equal "#t"
    "(pair? '())" |> rep |> should equal "#f"

[<Fact>]
let cons () =
    "(cons 'a '())" |> rep |> should equal "(a)"
    "(cons '(a) '(b c d))" |> rep |> should equal "((a) b c d)"
    "(cons \"a\" '(b c))" |> rep |> should equal "(\"a\" b c)"
    "(cons 'a 3)" |> rep |> should equal "(a . 3)"
    "(cons '(a b) 'c)" |> rep |> should equal "((a b) . c)"
    "(cons 'a '(b . c))" |> rep |> should equal "(a b . c)"

[<Fact>]
let car () =
    "(car '(a b c))" |> rep |> should equal "a"
    "(car '((a) b c d))" |> rep |> should equal "(a)"
    "(car '(1 . 2))" |> rep |> should equal "1"
    "(car '(1 2 . 3))" |> rep |> should equal "1"

[<Fact>]
let cdr () =
    "(cdr '(a b c))" |> rep |> should equal "(b c)"
    "(cdr '((a) b c d))" |> rep |> should equal "(b c d)"
    "(cdr '(1 . 2))" |> rep |> should equal "2"
    "(cdr '(1 2 . 3))" |> rep |> should equal "(2 . 3)"

[<Fact>]
let ``null?`` () =
    "(null? '(a . b))" |> rep |> should equal "#f"
    "(null? '(a b c))" |> rep |> should equal "#f"
    "(null? '())" |> rep |> should equal "#t"

[<Fact>]
let ``list?`` () =
    "(list? '(a . b))" |> rep |> should equal "#f"
    "(list? '(a b c))" |> rep |> should equal "#t"
    "(list? '())" |> rep |> should equal "#t"

[<Fact>]
let list () =
    "(list 'a (+ 3 4) 'c)" |> rep |> should equal "(a 7 c)"
    "(list)" |> rep |> should equal "()"

[<Fact>]
let append () =
    "(append '(x) '(y))" |> rep |> should equal "(x y)"
    "(append '(a) '(b c d))" |> rep |> should equal "(a b c d)"
    "(append '(a (b)) '((c)))" |> rep |> should equal "(a (b) (c))"
    "(append '(a b) '(c . d))" |> rep |> should equal "(a b c . d)"
    "(append '() 'a)" |> rep |> should equal "a"
    "(append '(a) '() '(b))" |> rep |> should equal "(a b)"
    "(append)" |> rep |> should equal "()"
    "(append '(1 2) '(3 4) '(5 6))" |> rep |> should equal "(1 2 3 4 5 6)"
    "(append '())" |> rep |> should equal "()"
    "(append 1)" |> rep |> should equal "1"
    "(append '(1) 2)" |> rep |> should equal "(1 . 2)"
    "(append '() '(1))" |> rep |> should equal "(1)"
    "(append '(1) '() '())" |> rep |> should equal "(1)"
    "(append '() '() '())" |> rep |> should equal "()"
    "(append '(1) '(2) '())" |> rep |> should equal "(1 2)"
