module WriteScheme.Tests.ListTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext

[<Fact>]
let ``pair?`` () =
    "(pair? '(a . b))" |> rep |> should equal "#t"
    "(pair? '(a b c))" |> rep |> should equal "#t"
    "(pair? '())" |> rep |> should equal "#f"

    "(pair? 1 2)" |> rep |> should startWith "'(1 2)' invalid pair? parameter"

[<Fact>]
let cons () =
    "(cons 'a '())" |> rep |> should equal "(a)"
    "(cons '(a) '(b c d))" |> rep |> should equal "((a) b c d)"
    "(cons \"a\" '(b c))" |> rep |> should equal "(\"a\" b c)"
    "(cons 'a 3)" |> rep |> should equal "(a . 3)"
    "(cons '(a b) 'c)" |> rep |> should equal "((a b) . c)"
    "(cons 'a '(b . c))" |> rep |> should equal "(a b . c)"

    "(cons 1)" |> rep |> should startWith "'(1)' invalid cons parameter"

[<Fact>]
let car () =
    "(car '(a b c))" |> rep |> should equal "a"
    "(car '((a) b c d))" |> rep |> should equal "(a)"
    "(car '(1 . 2))" |> rep |> should equal "1"
    "(car '(1 2 . 3))" |> rep |> should equal "1"

    "(car)" |> rep |> should startWith "'()' invalid car parameter"
    "(car 1 2)" |> rep |> should startWith "'(1 2)' invalid car parameter"
    "(car 'a)" |> rep |> should startWith "'a' invalid car parameter"

[<Fact>]
let cdr () =
    "(cdr '(a b c))" |> rep |> should equal "(b c)"
    "(cdr '((a) b c d))" |> rep |> should equal "(b c d)"
    "(cdr '(1 . 2))" |> rep |> should equal "2"
    "(cdr '(1 2 . 3))" |> rep |> should equal "(2 . 3)"

    "(cdr)" |> rep |> should startWith "'()' invalid cdr parameter"
    "(cdr 1 2)" |> rep |> should startWith "'(1 2)' invalid cdr parameter"
    "(cdr 'a)" |> rep |> should startWith "'a' invalid cdr parameter"

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

    "(set-car! 1)" |> rep |> should startWith "'(1)' invalid set-car! parameter"

    "(set-car! 1 2 3)"
    |> rep
    |> should startWith "'(1 2 3)' invalid set-car! parameter"

[<Fact>]
let ``set-cdr!`` () =
    "(let ((x (list 'a 'b 'c))) (set-cdr! x 'z) x)" |> rep |> should equal "(a . z)"
    "(let ((x (cons 'a 'b))) (set-cdr! x 'z) x)" |> rep |> should equal "(a . z)"
    "(let ((x (list 'a))) (set-cdr! x x) (car x))" |> rep |> should equal "a"
    "(let ((x (list 'a))) (set-cdr! x x) x)" |> rep |> should equal "(a ...)"

    "(let ((x (list 1 2))) (set-cdr! (cdr x) x) x)"
    |> rep
    |> should equal "(1 2 ...)"

    "(let ((x (list 1 2)) (y (list 3 4))) (set-cdr! (cdr x) y) (set-cdr! (cdr y) x) x)"
    |> rep
    |> should equal "(1 2 3 4 ...)"

    "(set-cdr! 1)" |> rep |> should startWith "'(1)' invalid set-cdr! parameter"

    "(set-cdr! 1 2 3)"
    |> rep
    |> should startWith "'(1 2 3)' invalid set-cdr! parameter"

[<Fact>]
let ``c...r`` () =
    "(caar '((1 2) 3))" |> rep |> should equal "1"
    "(cadr '(1 2 3))" |> rep |> should equal "2"
    "(cdar '((1 2) 3))" |> rep |> should equal "(2)"
    "(cddr '(1 2 3))" |> rep |> should equal "(3)"

    "(caar)" |> rep |> should startWith "'()' invalid caar parameter"
    "(caar)" |> rep |> should startWith "'()' invalid caar parameter"
    "(caar 1 2)" |> rep |> should startWith "'(1 2)' invalid caar parameter"
    "(caar 1)" |> rep |> should startWith "'1' invalid car parameter"

    "(cadr)" |> rep |> should startWith "'()' invalid cadr parameter"
    "(cadr 1 2)" |> rep |> should startWith "'(1 2)' invalid cadr parameter"
    "(cadr 1)" |> rep |> should startWith "'1' invalid cdr parameter"

    "(cdar)" |> rep |> should startWith "'()' invalid cdar parameter"
    "(cdar 1 2)" |> rep |> should startWith "'(1 2)' invalid cdar parameter"
    "(cdar 1)" |> rep |> should startWith "'1' invalid car parameter"

    "(cddr)" |> rep |> should startWith "'()' invalid cddr parameter"
    "(cddr 1 2)" |> rep |> should startWith "'(1 2)' invalid cddr parameter"
    "(cddr 1)" |> rep |> should startWith "'1' invalid cdr parameter"

[<Fact>]
let ``null?`` () =
    "(null? '(a . b))" |> rep |> should equal "#f"
    "(null? '(a b c))" |> rep |> should equal "#f"
    "(null? '())" |> rep |> should equal "#t"

    "(null? 1 2)" |> rep |> should startWith "'(1 2)' invalid null? parameter"

[<Fact>]
let ``list?`` () =
    "(list? '(a . b))" |> rep |> should equal "#f"
    "(list? '(a b c))" |> rep |> should equal "#t"
    "(list? '())" |> rep |> should equal "#t"
    "(let ((x (list 'a))) (set-cdr! x x) (list? x))" |> rep |> should equal "#f"

    "(list? 1 2)" |> rep |> should startWith "'(1 2)' invalid list? parameter"

[<Fact>]
let ``make-list`` () =
    "(make-list 2 3)" |> rep |> should equal "(3 3)"
    "(make-list 3 'a)" |> rep |> should equal "(a a a)"
    "(make-list 1)" |> rep |> should equal "(#<unspecified>)"
    "(make-list -1)" |> rep |> should startWith "'(-1)' invalid make-list parameter"

    "(make-list 2.5)"
    |> rep
    |> should startWith "'(2.5)' invalid make-list parameter"

    "(make-list)" |> rep |> should startWith "'()' invalid make-list parameter"

    "(make-list 1 2 3)"
    |> rep
    |> should startWith "'(1 2 3)' invalid make-list parameter"

[<Fact>]
let list () =
    "(list 'a (+ 3 4) 'c)" |> rep |> should equal "(a 7 c)"
    "(list)" |> rep |> should equal "()"

[<Fact>]
let ``length`` () =
    "(length '(a b c))" |> rep |> should equal "3"
    "(length '(a (b) (c d e)))" |> rep |> should equal "3"
    "(length '())" |> rep |> should equal "0"

    "(let ((x (list 'a))) (set-cdr! x x) (length x))"
    |> rep
    |> should startWith "'(a ...)' circular list."

    "(length 'a)" |> rep |> should startWith "'a' not a proper list."
    "(length '(a . b))" |> rep |> should startWith "'(a . b)' not a proper list."
    "(length)" |> rep |> should startWith "'()' invalid length parameter"
    "(length 1 2)" |> rep |> should startWith "'(1 2)' invalid length parameter"

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

    "(append '(1 . 2) '(3) '(4))" |> rep |> should startWith "not a proper list"

[<Fact>]
let ``reverse`` () =
    "(reverse '(a b c))" |> rep |> should equal "(c b a)"
    "(reverse '(a (b c) d (e (f))))" |> rep |> should equal "((e (f)) d (b c) a)"
    "(reverse '())" |> rep |> should equal "()"

    "(reverse 1 2)" |> rep |> should startWith "'(1 2)' invalid reverse parameter"

    "(reverse '(a . b))"
    |> rep
    |> should startWith "'b' is not a proper list in reverse"

[<Fact>]
let ``list-tail`` () =
    "(list-tail '(a b c d) 2)" |> rep |> should equal "(c d)"
    "(list-tail '(a b c d) 0)" |> rep |> should equal "(a b c d)"
    "(list-tail '() 0)" |> rep |> should equal "()"
    "(list-tail '(a b c d) 4)" |> rep |> should equal "()"
    "(list-tail '(1 2 . 3) 2)" |> rep |> should equal "3"
    "(list-tail '(a b . c) 1)" |> rep |> should equal "(b . c)"

    "(list-tail '(a b c) 5)"
    |> rep
    |> should startWith "'()' invalid list-tail parameter"

    "(list-tail 1 2 3)"
    |> rep
    |> should startWith "'(1 2 3)' invalid list-tail parameter"

[<Fact>]
let ``list-ref`` () =
    "(list-ref '(a b c d) 2)" |> rep |> should equal "c"
    "(list-ref '(a b c d) 0)" |> rep |> should equal "a"
    "(list-ref '(a b . c) 1)" |> rep |> should equal "b"
    "(list-ref '(a b c) 3)" |> rep |> should startWith "'()'"

    "(list-ref '(a b c) -1)"
    |> rep
    |> should startWith "'((a b c) -1)' invalid list-ref parameter"

    "(list-ref 1 2 3)"
    |> rep
    |> should startWith "'(1 2 3)' invalid list-ref parameter"

[<Fact>]
let ``list-set!`` () =
    "(let ((x (list 'a 'b 'c))) (list-set! x 1 'z) x)"
    |> rep
    |> should equal "(a z c)"

    "(let ((x (list 'a 'b 'c))) (list-set! x 3 'z))"
    |> rep
    |> should startWith "Out of range or not a pair in list-set!."

    "(let ((x (list 'a 'b 'c))) (list-set! x -1 'z))"
    |> rep
    |> should startWith "'((a b c) -1 z)' invalid list-set! parameter."

    "(list-set! 1 2)"
    |> rep
    |> should startWith "'(1 2)' invalid list-set! parameter"

[<Fact>]
let ``memq`` () =
    "(memq 'a '(a b c))" |> rep |> should equal "(a b c)"
    "(memq 'b '(a b c))" |> rep |> should equal "(b c)"
    "(memq 'a '(a . b))" |> rep |> should equal "(a . b)"
    "(memq 'a '(b c d))" |> rep |> should equal "#f"
    "(memq 'b '(a . b))" |> rep |> should equal "#f"
    "(memq (list 'a) '(b (a) c))" |> rep |> should equal "#f"
    "(memq 'a 'b)" |> rep |> should equal "#f"
    "(memq 'a '(a b . c))" |> rep |> should equal "(a b . c)"
    "(memq 'c '(a b . c))" |> rep |> should equal "#f"

    "(memq 1)" |> rep |> should startWith "'(1)' invalid memq parameter"

[<Fact>]
let ``memv`` () =
    "(memv 101 '(100 101 102))" |> rep |> should equal "(101 102)"

    "(memv 1)" |> rep |> should startWith "'(1)' invalid memv parameter"

[<Fact>]
let ``member`` () =
    "(member (list 'a) '(b (a) c))" |> rep |> should equal "((a) c)"

    "(member \"B\" '(\"a\" \"b\" \"c\") string-ci=?)"
    |> rep
    |> should equal "(\"b\" \"c\")"

    "(member 2.0 '(1 2 3) =)" |> rep |> should equal "(2 3)"
    "(member 2.0 '(1 2 3) eqv?)" |> rep |> should equal "#f"
    "(member 'a 'b)" |> rep |> should equal "#f"
    "(member 1 'a eqv?)" |> rep |> should equal "#f"
    "(member 1 '(2 3) (lambda (x y) #f))" |> rep |> should equal "#f"

    "(member 1)" |> rep |> should startWith "'(1)' invalid member parameter"

    "(member 1 '(2 3) (lambda (x y) (car 1)))"
    |> rep
    |> should startWith "'1' invalid car parameter"

[<Fact>]
let ``assq`` () =
    "(assq 'a '((a 1) (b 2) (c 3)))" |> rep |> should equal "(a 1)"
    "(assq 'b '((a 1) (b 2)))" |> rep |> should equal "(b 2)"
    "(assq 'a '((a 1) . b))" |> rep |> should equal "(a 1)"
    "(assq 'c '((a 1) (b 2)))" |> rep |> should equal "#f"
    "(assq 'b '((a 1) . b))" |> rep |> should equal "#f"
    "(assq (list 'a) '(((a)) ((b)) ((c))))" |> rep |> should equal "#f"
    "(assq 'a 'b)" |> rep |> should equal "#f"
    "(assq 'a '((a 1) (b 2) . c))" |> rep |> should equal "(a 1)"
    "(assq 'c '((a 1) (b 2) . c))" |> rep |> should equal "#f"

    "(assq 1)" |> rep |> should startWith "'(1)' invalid assq parameter"

[<Fact>]
let ``assv`` () =
    "(assv 5 '((2 3) (5 7) (11 13)))" |> rep |> should equal "(5 7)"

    "(assv 1)" |> rep |> should startWith "'(1)' invalid assv parameter"

[<Fact>]
let ``assoc`` () =
    "(assoc (list 'a) '(((a)) ((b)) ((c))))" |> rep |> should equal "((a))"

    "(assoc 2.0 '((1 \"a\") (2 \"b\") (3 \"c\")) =)"
    |> rep
    |> should equal "(2 \"b\")"

    "(assoc \"B\" '((\"a\" 1) (\"b\" 2)) string-ci=?)"
    |> rep
    |> should equal "(\"b\" 2)"

    "(assoc 'a 'b)" |> rep |> should equal "#f"
    "(assoc 1 '((2)) (lambda (x y) #f))" |> rep |> should equal "#f"
    "(assoc 2 '((2 3)) (lambda (x y) (= x y)))" |> rep |> should equal "(2 3)"
    "(assoc 1 2 eqv?)" |> rep |> should equal "#f"

    "(assoc 1)" |> rep |> should startWith "'(1)' invalid assoc parameter"
    "(assoc 1 '(2))" |> rep |> should startWith "'2' invalid car parameter"

    "(assoc 1 '((2 3)) (lambda (x y) (car 1)))"
    |> rep
    |> should startWith "'1' invalid car parameter"

    "(assoc 1 '(2) eqv?)" |> rep |> should startWith "'2' invalid car parameter"

[<Fact>]
let ``list-copy`` () =
    "(list-copy '(a b c))" |> rep |> should equal "(a b c)"
    "(list-copy '(a b . c))" |> rep |> should equal "(a b . c)"
    "(list-copy 'a)" |> rep |> should equal "a"
    "(list-copy '())" |> rep |> should equal "()"

    "(list-copy 1 2)"
    |> rep
    |> should startWith "'(1 2)' invalid list-copy parameter"
