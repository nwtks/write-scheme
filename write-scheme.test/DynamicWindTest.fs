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
let ``dynamic-wind nested jump`` () =
    let rep = repEnvs ()

    "(define path '())" |> rep |> ignore
    "(define k #f)" |> rep |> ignore

    "(dynamic-wind
      (lambda () (set! path (append path '(in-outer))))
      (lambda ()
        (if (call/cc (lambda (k1) (set! k k1) #t))
            (dynamic-wind
              (lambda () (set! path (append path '(in-inner1))))
              (lambda ()
                 (set! path (append path '(body1)))
                 (if k (let ((tmp k)) (set! k #f) (tmp #f))))
              (lambda () (set! path (append path '(out-inner1)))))
            (dynamic-wind
              (lambda () (set! path (append path '(in-inner2))))
              (lambda () (set! path (append path '(body2))))
              (lambda () (set! path (append path '(out-inner2)))))))
      (lambda () (set! path (append path '(out-outer)))))"
    |> rep
    |> ignore

    "path"
    |> rep
    |> should equal "(in-outer in-inner1 body1 out-inner1 in-inner2 body2 out-inner2 out-outer)"

[<Fact>]
let ``dynamic-wind different stacks jump`` () =
    let rep = repEnvs ()

    "(define path '())" |> rep |> ignore
    "(define k #f)" |> rep |> ignore

    "(dynamic-wind
        (lambda () (set! path (append path '(in1))))
        (lambda () (call/cc (lambda (k1) (set! k k1))))
        (lambda () (set! path (append path '(out1)))))"
    |> rep
    |> ignore

    "(dynamic-wind
        (lambda () (set! path (append path '(in2))))
        (lambda () (if k (let ((tmp k)) (set! k #f) (tmp #f))))
        (lambda () (set! path (append path '(out2)))))"
    |> rep
    |> ignore

    "path" |> rep |> should equal "(in1 out1 in2 out2 in1 out1)"
