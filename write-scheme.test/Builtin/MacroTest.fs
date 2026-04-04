module WriteScheme.Tests.MacroTest

open Xunit
open FsUnit.Xunit
open WriteScheme.Repl

[<Fact>]
let ``syntax-rules basic`` () =
    let envs = newEnvs ()

    "(define-syntax my-if
       (syntax-rules ()
         ((my-if test then else)
          (cond (test then) (#t else)))))"
    |> rep envs
    |> ignore

    "(my-if #t 1 2)" |> rep envs |> should equal "1"
    "(my-if #f 1 2)" |> rep envs |> should equal "2"
    "(my-if (> 3 2) 'yes 'no)" |> rep envs |> should equal "yes"

[<Fact>]
let ``syntax-rules ellipsis`` () =
    let envs = newEnvs ()

    "(define-syntax my-list
       (syntax-rules ()
         ((my-list x ...) (list x ...))))"
    |> rep envs
    |> ignore

    "(my-list 1 2 3)" |> rep envs |> should equal "(1 2 3)"
    "(my-list)" |> rep envs |> should equal "()"
    "(my-list 'a)" |> rep envs |> should equal "(a)"

[<Fact>]
let ``syntax-rules multiple rules`` () =
    let envs = newEnvs ()

    "(define-syntax my-and
       (syntax-rules ()
         ((my-and) #t)
         ((my-and x) x)
         ((my-and x y ...) (if x (my-and y ...) #f))))"
    |> rep envs
    |> ignore

    "(my-and)" |> rep envs |> should equal "#t"
    "(my-and 1)" |> rep envs |> should equal "1"
    "(my-and 1 2)" |> rep envs |> should equal "2"
    "(my-and #f 2)" |> rep envs |> should equal "#f"
    "(my-and 1 2 3)" |> rep envs |> should equal "3"

[<Fact>]
let ``syntax-rules swap!`` () =
    let envs = newEnvs ()

    "(define-syntax swap!
       (syntax-rules ()
         ((swap! a b)
          (let ((tmp a))
            (set! a b)
            (set! b tmp)))))"
    |> rep envs
    |> ignore

    "(define x 1)" |> rep envs |> ignore
    "(define y 2)" |> rep envs |> ignore
    "(swap! x y)" |> rep envs |> ignore
    "x" |> rep envs |> should equal "2"
    "y" |> rep envs |> should equal "1"

[<Fact>]
let ``syntax-rules literal keywords`` () =
    let envs = newEnvs ()

    "(define-syntax my-cond
       (syntax-rules (else)
         ((my-cond (else e)) e)
         ((my-cond (t e)) (if t e))))"
    |> rep envs
    |> ignore

    "(my-cond (#t 42))" |> rep envs |> should equal "42"
    "(my-cond (else 99))" |> rep envs |> should equal "99"
