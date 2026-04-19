module WriteScheme.Tests.ProcedureTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtin

[<Fact>]
let ``procedure?`` () =
    "(procedure? car)" |> rep |> should equal "#t"
    "(procedure? 'car)" |> rep |> should equal "#f"
    "(procedure? (lambda (x) (* x x)))" |> rep |> should equal "#t"
    "(procedure? '(lambda (x) (* x x)))" |> rep |> should equal "#f"
    "(call-with-current-continuation procedure?)" |> rep |> should equal "#t"

[<Fact>]
let ``apply`` () =
    "(apply + (list 3 4))" |> rep |> should equal "7"
    "(apply + 1 2 '(3))" |> rep |> should equal "6"
    "(apply + '())" |> rep |> should equal "0"
    "(apply list 1 '() '(2))" |> rep |> should equal "(1 () 2)"

[<Fact>]
let ``map`` () =
    "(map cdr '((a b) (d e) (g h)))" |> rep |> should equal "((b) (e) (h))"
    "(map + '(1 2 3) '(4 5 6 7))" |> rep |> should equal "(5 7 9)"
    "(map car '())" |> rep |> should equal "()"
    "(map (lambda (x) (* x x)) '(1 2 3))" |> rep |> should equal "(1 4 9)"

[<Fact>]
let ``string-map`` () =
    "(string-map (lambda (x) (integer->char (+ 1 (char->integer x)))) \"HAL\")"
    |> rep
    |> should equal "\"IBM\""

    "(string-map (lambda (x) x) \"🍎\")" |> rep |> should equal "\"🍎\""
    "(string-map (lambda (x y) x) \"🍎a\" \"bc\")" |> rep |> should equal "\"🍎a\""

[<Fact>]
let ``vector-map`` () =
    "(vector-map + '#(1 2 3) '#(4 5 6))" |> rep |> should equal "#(5 7 9)"
    "(vector-map (lambda (x) (* x x)) '#(1 2 3))" |> rep |> should equal "#(1 4 9)"

[<Fact>]
let ``for-each`` () =
    "(let
       ((v '()))
       (for-each
         (lambda (i) (set! v (cons (* i i) v)))
         '(0 1 2 3 4))
       v)"
    |> rep
    |> should equal "(16 9 4 1 0)"

    "(let ((v '())) (for-each (lambda (x) (set! v (cons x v))) '()) v)"
    |> rep
    |> should equal "()"

[<Fact>]
let ``string-for-each`` () =
    "(let ((v '())) (string-for-each (lambda (c) (set! v (cons c v))) \"abc\") v)"
    |> rep
    |> should equal "(#\\c #\\b #\\a)"

    "(begin (define sum 0) (string-for-each (lambda (x) (set! sum (+ sum (char->integer x)))) \"ABC\") sum)"
    |> rep
    |> should equal "198"

    "(begin (define last #f) (string-for-each (lambda (x) (set! last x)) \"🍎\") last)"
    |> rep
    |> should equal "#\\🍎"

    "(begin (define count 0) (string-for-each (lambda (x y) (set! count (+ count 1))) \"🍎a\" \"bc\") count)"
    |> rep
    |> should equal "2"

[<Fact>]
let ``vector-for-each`` () =
    "(let ((v (make-vector 3))) (vector-for-each (lambda (i x) (vector-set! v i (* x x))) '#(0 1 2) '#(1 2 3)) v)"
    |> rep
    |> should equal "#(1 4 9)"

[<Fact>]
let ``call-with-current-continuation`` () =
    "(define list-length
       (lambda (obj)
         (call-with-current-continuation
           (lambda (return)
             (letrec
               ((r
                 (lambda (o)
                   (cond
                     ((null? o) 0)
                     ((pair? o) (+ (r (cdr o)) 1))
                     (else (return #f))))))
               (r obj))))))"
    |> rep
    |> ignore

    "(list-length '(a b c d))" |> rep |> should equal "4"
    "(list-length '(a b . c))" |> rep |> should equal "#f"

[<Fact>]
let ``values`` () =
    "(call-with-values (lambda () (values 1 2 3)) list)"
    |> rep
    |> should equal "(1 2 3)"

    "(values 42)" |> rep |> should equal "42"
