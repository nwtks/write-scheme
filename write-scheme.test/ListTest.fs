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
let ``set-car!`` () =
    "(let ((x (list 'a 'b 'c))) (set-car! x 'z) x)" |> rep |> should equal "(z b c)"
    "(let ((x (cons 'a 'b))) (set-car! x 'z) x)" |> rep |> should equal "(z . b)"

    "(let* ((x (list 'a 'b 'c)) (y x)) (set-car! x 'z) y)"
    |> rep
    |> should equal "(z b c)"

    "(let* ((x (list 'a 'b 'c)) (y (cdr x))) (set-car! y 'z) x)"
    |> rep
    |> should equal "(a z c)"

[<Fact>]
let ``set-cdr!`` () =
    "(let ((x (list 'a 'b 'c))) (set-cdr! x 'z) x)" |> rep |> should equal "(a . z)"
    "(let ((x (cons 'a 'b))) (set-cdr! x 'z) x)" |> rep |> should equal "(a . z)"
    "(let ((x (list 'a))) (set-cdr! x x) (car x))" |> rep |> should equal "a"
    "(let ((x (list 'a))) (set-cdr! x x) x)" |> rep |> should equal "(a ...)"

[<Fact>]
let ``c...r`` () =
    "(caar '((1 2) 3))" |> rep |> should equal "1"
    "(cadr '(1 2 3))" |> rep |> should equal "2"
    "(cdar '((1 2) 3))" |> rep |> should equal "(2)"
    "(cddr '(1 2 3))" |> rep |> should equal "(3)"

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
let ``make-list`` () =
    "(make-list 2 3)" |> rep |> should equal "(3 3)"
    "(make-list 3 'a)" |> rep |> should equal "(a a a)"
    "(make-list 1)" |> rep |> should equal "(#<unspecified>)"

[<Fact>]
let list () =
    "(list 'a (+ 3 4) 'c)" |> rep |> should equal "(a 7 c)"
    "(list)" |> rep |> should equal "()"

[<Fact>]
let ``length`` () =
    "(length '(a b c))" |> rep |> should equal "3"
    "(length '(a (b) (c d e)))" |> rep |> should equal "3"
    "(length '())" |> rep |> should equal "0"

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

[<Fact>]
let ``reverse`` () =
    "(reverse '(a b c))" |> rep |> should equal "(c b a)"
    "(reverse '(a (b c) d (e (f))))" |> rep |> should equal "((e (f)) d (b c) a)"
    "(reverse '())" |> rep |> should equal "()"

[<Fact>]
let ``list-tail`` () =
    "(list-tail '(a b c d) 2)" |> rep |> should equal "(c d)"
    "(list-tail '(a b c d) 0)" |> rep |> should equal "(a b c d)"
    "(list-tail '() 0)" |> rep |> should equal "()"
    "(list-tail '(a b c d) 4)" |> rep |> should equal "()"
    "(list-tail '(1 2 . 3) 2)" |> rep |> should equal "3"
    "(list-tail '(a b . c) 1)" |> rep |> should equal "(b . c)"

[<Fact>]
let ``list-ref`` () =
    "(list-ref '(a b c d) 2)" |> rep |> should equal "c"
    "(list-ref '(a b c d) 0)" |> rep |> should equal "a"
    "(list-ref '(a b . c) 1)" |> rep |> should equal "b"

[<Fact>]
let ``memq`` () =
    "(memq 'a '(a b c))" |> rep |> should equal "(a b c)"
    "(memq 'b '(a b c))" |> rep |> should equal "(b c)"
    "(memq 'a '(a . b))" |> rep |> should equal "(a . b)"
    "(memq 'a '(b c d))" |> rep |> should equal "#f"
    "(memq 'b '(a . b))" |> rep |> should equal "#f"
    "(memq (list 'a) '(b (a) c))" |> rep |> should equal "#f"

[<Fact>]
let ``memv`` () =
    "(memv 101 '(100 101 102))" |> rep |> should equal "(101 102)"

[<Fact>]
let ``member`` () =
    "(member (list 'a) '(b (a) c))" |> rep |> should equal "((a) c)"

[<Fact>]
let ``assq`` () =
    "(assq 'a '((a 1) (b 2) (c 3)))" |> rep |> should equal "(a 1)"
    "(assq 'b '((a 1) (b 2)))" |> rep |> should equal "(b 2)"
    "(assq 'a '((a 1) . b))" |> rep |> should equal "(a 1)"
    "(assq 'c '((a 1) (b 2)))" |> rep |> should equal "#f"
    "(assq 'b '((a 1) . b))" |> rep |> should equal "#f"
    "(assq (list 'a) '(((a)) ((b)) ((c))))" |> rep |> should equal "#f"

[<Fact>]
let ``assv`` () =
    "(assv 5 '((2 3) (5 7) (11 13)))" |> rep |> should equal "(5 7)"

[<Fact>]
let ``assoc`` () =
    "(assoc (list 'a) '(((a)) ((b)) ((c))))" |> rep |> should equal "((a))"

[<Fact>]
let ``list-copy`` () =
    "(list-copy '(a b c))" |> rep |> should equal "(a b c)"
    "(list-copy '(a b . c))" |> rep |> should equal "(a b . c)"
    "(list-copy 'a)" |> rep |> should equal "a"
    "(list-copy '())" |> rep |> should equal "()"
