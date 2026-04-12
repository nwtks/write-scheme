module WriteScheme.Tests.DynamicWindTest

open Xunit
open FsUnit.Xunit

let repEnvs () =
    WriteScheme.Repl.newEnvs () |> WriteScheme.Repl.rep

[<Fact>]
let ``dynamic-wind basic`` () =
    let rep = repEnvs ()

    "(define path '())" |> rep |> ignore

    "(dynamic-wind
        (lambda () (set! path (cons 'in path)))
        (lambda () (set! path (cons 'body path)) 'result)
        (lambda () (set! path (cons 'out path))))"
    |> rep
    |> should equal "result"

    "path" |> rep |> should equal "(out body in)"

[<Fact>]
let ``dynamic-wind with call/cc`` () =
    let rep = repEnvs ()

    "(define path '())" |> rep |> ignore
    "(define c #f)" |> rep |> ignore

    "(let ((ans
            (dynamic-wind
                (lambda () (set! path (cons 'in path)))
                (lambda ()
                    (set! path (cons 'body1 path))
                    (call/cc (lambda (k) (set! c k) 'from-body1)))
                (lambda () (set! path (cons 'out path))))))
        (set! path (cons ans path)))"
    |> rep
    |> ignore

    "path" |> rep |> should equal "(from-body1 out body1 in)"

    "(define p2 path)" |> rep |> ignore
    "(set! path '())" |> rep |> ignore
    "(c \"second-time\")" |> rep |> ignore
    "path" |> rep |> should equal "(\"second-time\" out in)"

[<Fact>]
let ``dynamic-wind with exception handler`` () =
    let rep = repEnvs ()

    "(define path '())" |> rep |> ignore

    "(with-exception-handler
        (lambda (e) (set! path (cons e path)))
        (lambda ()
            (dynamic-wind
                (lambda () (set! path (cons 'in path)))
                (lambda () (raise 'error))
                (lambda () (set! path (cons 'out path))))))"
    |> rep
    |> ignore

    "path" |> rep |> should equal "(out error in)"

[<Fact>]
let ``parameterize basic`` () =
    let rep = repEnvs ()

    "(define p (make-parameter 0))" |> rep |> ignore
    "(parameterize ((p 1)) (p))" |> rep |> should equal "1"
    "(p)" |> rep |> should equal "0"

[<Fact>]
let ``parameterize with call/cc`` () =
    let rep = repEnvs ()

    "(define p (make-parameter 0))" |> rep |> ignore
    "(define c #f)" |> rep |> ignore

    "(define (test)
        (parameterize ((p 1))
            (call/cc (lambda (k) (set! c k)))
            (p)))"
    |> rep
    |> ignore

    "(test)" |> rep |> should equal "1"
    "(p)" |> rep |> should equal "0"

    "(c #t)" |> rep |> should equal "1"
    "(p)" |> rep |> should equal "0"

    "(p 2)" |> rep |> ignore
    "(c #t)" |> rep |> should equal "1"
    "(p)" |> rep |> should equal "2"
