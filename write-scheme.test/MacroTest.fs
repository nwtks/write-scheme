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
