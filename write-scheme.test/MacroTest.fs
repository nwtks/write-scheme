module WriteScheme.Tests.MacroTest

open Xunit
open FsUnit.Xunit

let repEnvs () =
    WriteScheme.Repl.newEnvs () |> WriteScheme.Repl.rep

[<Fact>]
let ``syntax-rules basic`` () =
    let rep = repEnvs ()

    "(define-syntax my-if
       (syntax-rules ()
         ((my-if test then else)
          (cond (test then) (#t else)))))"
    |> rep
    |> ignore

    "(my-if #t 1 2)" |> rep |> should equal "1"
    "(my-if #f 1 2)" |> rep |> should equal "2"
    "(my-if (> 3 2) 'yes 'no)" |> rep |> should equal "yes"

[<Fact>]
let ``syntax-rules ellipsis`` () =
    let rep = repEnvs ()

    "(define-syntax my-list
       (syntax-rules ()
         ((my-list x ...) (list x ...))))"
    |> rep
    |> ignore

    "(my-list 1 2 3)" |> rep |> should equal "(1 2 3)"
    "(my-list)" |> rep |> should equal "()"
    "(my-list 'a)" |> rep |> should equal "(a)"

[<Fact>]
let ``syntax-rules multiple rules`` () =
    let rep = repEnvs ()

    "(define-syntax my-and
       (syntax-rules ()
         ((my-and) #t)
         ((my-and x) x)
         ((my-and x y ...) (if x (my-and y ...) #f))))"
    |> rep
    |> ignore

    "(my-and)" |> rep |> should equal "#t"
    "(my-and 1)" |> rep |> should equal "1"
    "(my-and 1 2)" |> rep |> should equal "2"
    "(my-and #f 2)" |> rep |> should equal "#f"
    "(my-and 1 2 3)" |> rep |> should equal "3"

[<Fact>]
let ``syntax-rules swap!`` () =
    let rep = repEnvs ()

    "(define-syntax swap!
       (syntax-rules ()
         ((swap! a b)
          (let ((tmp a))
            (set! a b)
            (set! b tmp)))))"
    |> rep
    |> ignore

    "(define x 1)" |> rep |> ignore
    "(define y 2)" |> rep |> ignore
    "(swap! x y)" |> rep |> ignore
    "x" |> rep |> should equal "2"
    "y" |> rep |> should equal "1"

[<Fact>]
let ``syntax-rules literal keywords`` () =
    let rep = repEnvs ()

    "(define-syntax my-cond
       (syntax-rules (else)
         ((my-cond (else e)) e)
         ((my-cond (t e)) (if t e))))"
    |> rep
    |> ignore

    "(my-cond (#t 42))" |> rep |> should equal "42"
    "(my-cond (else 99))" |> rep |> should equal "99"

[<Fact>]
let ``syntax-error`` () =
    let rep = repEnvs ()

    (fun () -> "(syntax-error \"test error\" 1 2)" |> rep |> ignore)
    |> should throw typeof<WriteScheme.Type.SchemeRaise>

    "(define-syntax check-positive
       (syntax-rules ()
         ((check-positive x)
          (if (> x 0) x (syntax-error \"not positive\" x)))))"
    |> rep
    |> ignore

    "(check-positive 1)" |> rep |> should equal "1"

    (fun () -> "(check-positive -1)" |> rep |> ignore)
    |> should throw typeof<WriteScheme.Type.SchemeRaise>

[<Fact>]
let ``syntax-rules hygiene shadowing`` () =
    let rep = repEnvs ()

    "(define-syntax swap-hygiene!
       (syntax-rules ()
         ((swap-hygiene! a b)
          (let ((tmp a))
            (set! a b)
            (set! b tmp)))))"
    |> rep
    |> ignore

    "(define x 1)" |> rep |> ignore
    "(define y 2)" |> rep |> ignore

    "(let ((tmp 5))
       (swap-hygiene! x y)
       tmp)"
    |> rep
    |> should equal "5"

    "x" |> rep |> should equal "2"
    "y" |> rep |> should equal "1"

[<Fact>]
let ``syntax-rules hygiene preservation`` () =
    let rep = repEnvs ()

    "(define-syntax my-if
       (syntax-rules ()
         ((my-if t a b) (if t a b))))"
    |> rep
    |> ignore

    "(let ((if #f))
       (my-if #t 1 2))"
    |> rep
    |> should equal "1"

[<Fact>]
let ``nested ellipsis`` () =
    let rep = repEnvs ()

    "(define-syntax nested
        (syntax-rules ()
            ((_ ((x ...) ...))
              (list (list x ...) ...))))"
    |> rep
    |> ignore

    "(nested ((1 2) (3 4)))" |> rep |> should equal "((1 2) (3 4))"

[<Fact>]
let ``deeply nested ellipsis`` () =
    let rep = repEnvs ()

    "(define-syntax nested-3
        (syntax-rules ()
            ((_ (((x ...) ...) ...))
              (list (list (list x ...) ...) ...))))"
    |> rep
    |> ignore

    "(nested-3 (((1 2) (3 4)) ((5 6) (7 8))))"
    |> rep
    |> should equal "(((1 2) (3 4)) ((5 6) (7 8)))"

[<Fact>]
let ``literal matching with shadowing`` () =
    let rep = repEnvs ()

    "(define-syntax check-lit
        (syntax-rules (lit)
            ((_ lit) #t)
            ((_ _) #f)))"
    |> rep
    |> ignore

    "(check-lit lit)" |> rep |> should equal "#t"

    // Shadowing 'lit' should make it NOT match the literal in the macro
    "(let ((lit 1)) (check-lit lit))" |> rep |> should equal "#f"

[<Fact>]
let ``dot pair pattern`` () =
    let rep = repEnvs ()

    "(define-syntax my-dot
        (syntax-rules ()
            ((_ (a . b)) (list a 'b))))"
    |> rep
    |> ignore

    "(my-dot (1 . 2))" |> rep |> should equal "(1 2)"
    "(my-dot (1 2 3))" |> rep |> should equal "(1 (2 3))"

[<Fact>]
let ``vector pattern`` () =
    let rep = repEnvs ()

    "(define-syntax my-vector
        (syntax-rules ()
            ((_ #(x y)) (list y x))))"
    |> rep
    |> ignore

    "(my-vector #(1 2))" |> rep |> should equal "(2 1)"

[<Fact>]
let ``ellipsis in middle of pattern`` () =
    let rep = repEnvs ()

    "(define-syntax head-tail
        (syntax-rules ()
            ((_ (h ... t1 t2)) (list (list h ...) t1 t2))))"
    |> rep
    |> ignore

    "(head-tail (1 2 3 4 5))" |> rep |> should equal "((1 2 3) 4 5)"
    "(head-tail (1 2))" |> rep |> should equal "(() 1 2)"

[<Fact>]
let ``custom ellipsis support`` () =
    let rep = repEnvs ()

    "(define-syntax my-list
        (syntax-rules ::: ()
            ((_ x :::) (list x :::))))"
    |> rep
    |> ignore

    "(my-list 1 2 3)" |> rep |> should equal "(1 2 3)"

[<Fact>]
let ``custom ellipsis with literals`` () =
    let rep = repEnvs ()

    "(define-syntax check-lit
        (syntax-rules ::: (lit)
            ((_ lit x :::) (list x :::))))"
    |> rep
    |> ignore

    "(check-lit lit 1 2 3)" |> rep |> should equal "(1 2 3)"

    (fun () -> "(let ((lit 0)) (check-lit lit 1 2 3))" |> rep |> ignore)
    |> should throw typeof<System.Exception>

[<Fact>]
let ``ellipsis escape in template`` () =
    let rep = repEnvs ()

    "(define-syntax escape-test
        (syntax-rules ()
            ((_) '(a (... ...)))))"
    |> rep
    |> ignore

    "(escape-test)" |> rep |> should endWith " ...)"

[<Fact>]
let ``ellipsis escape in pattern`` () =
    let rep = repEnvs ()

    "(define-syntax match-escape
        (syntax-rules ()
            ((_ (... ...)) 'matched-ellipsis)))"
    |> rep
    |> ignore

    "(match-escape ...)" |> rep |> should startWith "matched-ellipsis"

[<Fact>]
let ``ellipsis literal template`` () =
    let rep = repEnvs ()

    "(define-syntax lit-tmpl
        (syntax-rules ()
            ((_ x ...) (quote (... (x ...))))))"
    |> rep
    |> ignore

    "(lit-tmpl 1 2 3)" |> rep |> should equal "(x ...)"
