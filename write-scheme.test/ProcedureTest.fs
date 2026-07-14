module WriteScheme.Tests.ProcedureTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext

let newRep () =
    WriteScheme.Repl.newContext [] |> WriteScheme.Repl.rep

[<Fact>]
let ``procedure?`` () =
    let rep = newRep ()
    "(procedure? car)" |> rep |> should equal "#t"
    "(procedure? 'car)" |> rep |> should equal "#f"

    "(procedure? (lambda (x) (* x x)))" |> rep |> should equal "#t"
    "(procedure? '(lambda (x) (* x x)))" |> rep |> should equal "#f"

    "(procedure? (case-lambda ((x) x) ((x y) (+ x y))))" |> rep |> should equal "#t"

    "(call-with-current-continuation procedure?)" |> rep |> should equal "#t"
    "(procedure? if)" |> rep |> should equal "#f"

    "(procedure? (make-parameter 1))" |> rep |> should equal "#t"

    "(define-syntax my-macro (syntax-rules () ((my-macro) #t)))" |> rep |> ignore
    "(procedure? my-macro)" |> rep |> should equal "#f"

    "(procedure? 1 2)"
    |> rep
    |> should startWith "'(1 2)' invalid procedure? parameter"

[<Fact>]
let ``apply`` () =
    "(apply + (list 3 4))" |> rep |> should equal "7"
    "(apply + 1 2 '(3))" |> rep |> should equal "6"
    "(apply + '())" |> rep |> should equal "0"
    "(apply list 1 '() '(2))" |> rep |> should equal "(1 () 2)"
    "(apply + 1 2 3 '(4 5))" |> rep |> should equal "15"
    "(apply list 1 2 '())" |> rep |> should equal "(1 2)"
    "(apply (case-lambda ((x) 1) ((x y) 2)) '(a))" |> rep |> should equal "1"
    "(apply (case-lambda ((x) 1) ((x y) 2)) '(a b))" |> rep |> should equal "2"
    "(apply (lambda x x) 1 2 '(3 4))" |> rep |> should equal "(1 2 3 4)"
    "(apply apply (list + (list 1 2)))" |> rep |> should equal "3"

    "(apply (lambda (x . y) (list x y)) 1 2 '(3 4))"
    |> rep
    |> should equal "(1 (2 3 4))"

    "(apply + 1 2 3)" |> rep |> should startWith "'(1 2 3)' invalid apply parameter"
    "(apply)" |> rep |> should startWith "'()' invalid apply parameter"

[<Fact>]
let ``map`` () =
    "(map cdr '((a b) (d e) (g h)))" |> rep |> should equal "((b) (e) (h))"
    "(map + '(1 2 3) '(4 5 6 7))" |> rep |> should equal "(5 7 9)"
    "(map car '())" |> rep |> should equal "()"
    "(map (lambda (x) (* x x)) '(1 2 3))" |> rep |> should equal "(1 4 9)"
    "(map + '(1 2) '(3 4 5) '(6))" |> rep |> should equal "(10)"

    "(let ((cont #f))
       (let ((r (map (lambda (x) (if (= x 2) (call/cc (lambda (k) (set! cont k) x)) x)) '(1 2 3))))
         (if cont
             (let ((k cont))
               (set! cont #f)
               (k 20))
             r)))"
    |> rep
    |> should equal "(1 20 3)"

    "(let ((cont #f) (v '()))
       (map (lambda (x) (if (= x 2) (call/cc (lambda (k) (set! cont k)))) (set! v (cons x v))) '(1 2 3))
       (if cont
           (let ((k cont)) (set! cont #f) (k #f)))
       v)"
    |> rep
    |> should equal "(3 2 3 2 1)"

    "(map (lambda (x) (raise 1)) '(1 2))" |> rep |> should equal "1"

    "(map car)" |> rep |> should startWith "'(#<procedure>)' invalid map parameter"
    "(map)" |> rep |> should startWith "'()' invalid map parameter"
    "(map 1 '(1 2))" |> rep |> should startWith "'1' not operator"

[<Fact>]
let ``string-map`` () =
    "(string-map (lambda (x) (integer->char (+ 1 (char->integer x)))) \"HAL\")"
    |> rep
    |> should equal "\"IBM\""

    "(string-map (lambda (x y) (if (char<? x y) x y)) \"abc\" \"bca\")"
    |> rep
    |> should equal "\"aba\""

    "(string-map (lambda (x) x) \"🍎\")" |> rep |> should equal "\"🍎\""
    "(string-map (lambda (x y) x) \"🍎a\" \"bc\")" |> rep |> should equal "\"🍎a\""
    "(string-map (lambda (x y) x) \"abc\" \"de\")" |> rep |> should equal "\"ab\""

    "(string-map (lambda (x) (raise 1)) \"abc\")" |> rep |> should equal "1"

    "(string-map char-upcase)"
    |> rep
    |> should startWith "'(#<procedure>)' invalid string-map parameter"

    "(string-map)" |> rep |> should startWith "'()' invalid string-map parameter"
    "(string-map 1 \"abc\")" |> rep |> should startWith "'1' not operator"

[<Fact>]
let ``vector-map`` () =
    "(vector-map + '#(1 2 3) '#(4 5 6))" |> rep |> should equal "#(5 7 9)"
    "(vector-map (lambda (x) (* x x)) '#(1 2 3))" |> rep |> should equal "#(1 4 9)"
    "(vector-map + '#(1 2) '#(10 20 30))" |> rep |> should equal "#(11 22)"

    "(vector-map (lambda (x y) (+ x y)) '#(1 2 3) '#(10 20 30))"
    |> rep
    |> should equal "#(11 22 33)"

    "(vector-map (lambda (x) (raise 1)) '#(1 2))" |> rep |> should equal "1"

    "(vector-map vector-ref)"
    |> rep
    |> should startWith "'(#<procedure>)' invalid vector-map parameter"

    "(vector-map)" |> rep |> should startWith "'()' invalid vector-map parameter"
    "(vector-map 1 '#(1 2))" |> rep |> should startWith "'1' not operator"

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

    "(let ((v '())) (for-each (lambda (x y) (set! v (cons (+ x y) v))) '(1 2 3) '(10 20 30 40)) v)"
    |> rep
    |> should equal "(33 22 11)"

    "(for-each (lambda (x) (raise 1)) '(1 2))" |> rep |> should equal "1"

    "(for-each 1 '(1 2))" |> rep |> should startWith "'1' not operator"
    "(for-each)" |> rep |> should startWith "'()' invalid for-each parameter"

    "(for-each car)"
    |> rep
    |> should startWith "'(#<procedure>)' invalid for-each parameter"

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

    "(string-for-each)"
    |> rep
    |> should startWith "'()' invalid string-for-each parameter"

    "(string-for-each char-upcase)"
    |> rep
    |> should startWith "'(#<procedure>)' invalid string-for-each parameter"

    "(string-for-each 1 \"abc\")" |> rep |> should startWith "'1' not operator"

[<Fact>]
let ``vector-for-each`` () =
    "(let ((v (make-vector 3))) (vector-for-each (lambda (i x) (vector-set! v i (* x x))) '#(0 1 2) '#(1 2 3)) v)"
    |> rep
    |> should equal "#(1 4 9)"

    "(vector-for-each)"
    |> rep
    |> should startWith "'()' invalid vector-for-each parameter"

    "(vector-for-each vector-ref)"
    |> rep
    |> should startWith "'(#<procedure>)' invalid vector-for-each parameter"

    "(vector-for-each 1 '#(1 2))" |> rep |> should startWith "'1' not operator"

[<Fact>]
let ``call-with-current-continuation`` () =
    let rep = newRep ()

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

    "(call/cc (lambda (k) (k 1 2)))" |> rep |> should equal "(values 1 2)"

    "(call/cc)" |> rep |> should startWith "'()' invalid call/cc parameter"

    "(call/cc + 2)"
    |> rep
    |> should startWith "'(#<procedure> 2)' invalid call/cc parameter"

[<Fact>]
let ``call-with-values`` () =
    let rep = newRep ()

    "(call-with-values
       (lambda () (call-with-values (lambda () (values 1 2)) (lambda (a b) (values b a))))
       list)"
    |> rep
    |> should equal "(2 1)"

    "(call-with-values
       (lambda () (values 1 2))
       (lambda (a b) (values b a)))"
    |> rep
    |> should equal "(values 2 1)"

    "(call-with-values (lambda () (values 1 2)) +)" |> rep |> should equal "3"

    "(call-with-values (lambda () (values 4 5)) (lambda (a b) b))"
    |> rep
    |> should equal "5"

    "(call-with-values (lambda () 42) (lambda (x) x))" |> rep |> should equal "42"

    "(call-with-values (lambda () (begin 1 (values 2 3))) +)"
    |> rep
    |> should equal "5"

    "(call-with-values (lambda () (let ((x 1)) (values x 2))) +)"
    |> rep
    |> should equal "3"

    "(call-with-values (lambda () (if #t (values 1 2) 3)) +)"
    |> rep
    |> should equal "3"

    "(call-with-values (lambda () (do ((i 0 (+ i 1))) ((= i 3) (values 1 2)))) +)"
    |> rep
    |> should equal "3"

    "(call-with-values (lambda () (cond (#t => (lambda (x) (values 1 2))))) +)"
    |> rep
    |> should equal "3"

    "(call-with-values (lambda () (call/cc (lambda (k) (k 1 2)))) list)"
    |> rep
    |> should equal "(1 2)"

    "(call-with-values (lambda () (raise 1)) list)" |> rep |> should equal "1"

    "(call-with-values (lambda () (call/cc (lambda (k) (k)))) list)"
    |> rep
    |> should equal "()"

    "(call-with-values)"
    |> rep
    |> should startWith "'()' invalid call-with-values parameter"

    "(call-with-values +)"
    |> rep
    |> should startWith "'(#<procedure>)' invalid call-with-values parameter"

[<Fact>]
let ``values`` () =
    let rep = newRep ()

    "(values)" |> rep |> should equal "(values)"

    "(+ 1 (values 2 3))"
    |> rep
    |> should startWith "Multiple values in single value context"

    "(list (values 1 2))"
    |> rep
    |> should startWith "Multiple values in single value context"
