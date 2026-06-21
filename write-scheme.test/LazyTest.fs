module WriteScheme.Tests.LazyTest

open Xunit
open FsUnit.Xunit

let newRep () =
    WriteScheme.Repl.newContext () |> WriteScheme.Repl.rep

[<Fact>]
let ``delay`` () =
    let rep = newRep ()

    "(force (delay (+ 1 2)))" |> rep |> should equal "3"

    "(define count 0)" |> rep |> ignore
    "(define p (delay (begin (set! count (+ count 1)) count)))" |> rep |> ignore
    "(force p)" |> rep |> should equal "1"
    "(force p)" |> rep |> should equal "1"
    "(force 1)" |> rep |> should equal "1"
    "count" |> rep |> should equal "1"

[<Fact>]
let ``delay-force`` () =
    let rep = newRep ()

    "(force (delay-force (delay (delay-force (delay 10)))))"
    |> rep
    |> should equal "10"

[<Fact>]
let ``promise?`` () =
    let rep = newRep ()

    "(promise? 1)" |> rep |> should equal "#f"
    "(promise? (make-promise 1))" |> rep |> should equal "#t"

    "(define p (delay 42))" |> rep |> ignore
    "(promise? p)" |> rep |> should equal "#t"

[<Fact>]
let ``make-promise`` () =
    let rep = newRep ()

    "(force (make-promise 42))" |> rep |> should equal "42"
    "(make-promise (delay 1))" |> rep |> should equal "#<promise>"
