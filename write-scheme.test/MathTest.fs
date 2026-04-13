module WriteScheme.Tests.MathTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtin

[<Fact>]
let ``+`` () =
    "(+)" |> rep |> should equal "0"
    "(+ 10)" |> rep |> should equal "10"
    "(+ 10 2)" |> rep |> should equal "12"
    "(+ 10 2 3)" |> rep |> should equal "15"
    "(+ 1+2i 3+4i)" |> rep |> should equal "4+6i"

[<Fact>]
let ``*`` () =
    "(*)" |> rep |> should equal "1"
    "(* 10)" |> rep |> should equal "10"
    "(* 10 2)" |> rep |> should equal "20"
    "(* 10 2 3)" |> rep |> should equal "60"
    "(* 2 0+1i)" |> rep |> should equal "0+2i"

[<Fact>]
let ``-`` () =
    "(-)" |> rep |> should equal "0"
    "(- 10)" |> rep |> should equal "-10"
    "(- 10 2)" |> rep |> should equal "8"
    "(- 10 2 3)" |> rep |> should equal "5"
    "(- 1+2i 1+2i)" |> rep |> should equal "0+0i"

[<Fact>]
let ``/`` () =
    "(/)" |> rep |> should equal "1"
    "(/ 10)" |> rep |> should equal "1/10"
    "(/ 9 2)" |> rep |> should equal "9/2"
    "(/ 12 2 3)" |> rep |> should equal "2"
    "(/ 3 4 5)" |> rep |> should equal "3/20"

[<Fact>]
let predicates () =
    "(complex? 1+2i)" |> rep |> should equal "#t"
    "(complex? 1)" |> rep |> should equal "#t"
    "(real? 2.5)" |> rep |> should equal "#t"
    "(real? 2.5+0.0i)" |> rep |> should equal "#t"
    "(real? 2.5+1.0i)" |> rep |> should equal "#f"
    "(rational? 1/2)" |> rep |> should equal "#t"
    "(rational? 1.5)" |> rep |> should equal "#t"
    "(rational? +inf.0)" |> rep |> should equal "#f"
    "(integer? 3.0)" |> rep |> should equal "#t"
    "(integer? 3.2)" |> rep |> should equal "#f"
    "(integer? 3.0+0.0i)" |> rep |> should equal "#t"
    "(exact? 1)" |> rep |> should equal "#t"
    "(exact? 1.0)" |> rep |> should equal "#f"
    "(inexact? 1.0)" |> rep |> should equal "#t"
    "(exact-integer? 1)" |> rep |> should equal "#t"
    "(exact-integer? 1.0)" |> rep |> should equal "#f"
    "(finite? 3)" |> rep |> should equal "#t"
    "(finite? +inf.0)" |> rep |> should equal "#f"
    "(infinite? -inf.0)" |> rep |> should equal "#t"
    "(nan? +nan.0)" |> rep |> should equal "#t"
    "(odd? 3)" |> rep |> should equal "#t"
    "(odd? 3.0)" |> rep |> should equal "#t"
    "(even? 2)" |> rep |> should equal "#t"
    "(even? 2.0)" |> rep |> should equal "#t"

[<Fact>]
let complex_math () =
    "(= 1+2i (+ 1 0+2i))" |> rep |> should equal "#t"
    "(= 1 1+0i)" |> rep |> should equal "#t"
    "(real-part 3+4i)" |> rep |> should equal "3"
    "(imag-part 3+4i)" |> rep |> should equal "4"
    "(magnitude 3+4i)" |> rep |> should equal "5"
    "(= 0 (angle 1+0i))" |> rep |> should equal "#t"
