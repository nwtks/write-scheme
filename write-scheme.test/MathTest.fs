module WriteScheme.Tests.MathTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext

[<Fact>]
let ``number?`` () =
    "(number? 1)" |> rep |> should equal "#t"
    "(number? 1.0)" |> rep |> should equal "#t"
    "(number? 1+2i)" |> rep |> should equal "#t"
    "(number? \"a\")" |> rep |> should equal "#f"

[<Fact>]
let ``complex?`` () =
    "(complex? 1+2i)" |> rep |> should equal "#t"
    "(complex? 1)" |> rep |> should equal "#t"
    "(complex? 1.0)" |> rep |> should equal "#t"

[<Fact>]
let ``real?`` () =
    "(real? 2.5)" |> rep |> should equal "#t"
    "(real? 2.5+0.0i)" |> rep |> should equal "#t"
    "(real? 2.5+1.0i)" |> rep |> should equal "#f"
    "(real? 1)" |> rep |> should equal "#t"
    "(real? 1+0i)" |> rep |> should equal "#t"

[<Fact>]
let ``rational?`` () =
    "(rational? 1/2)" |> rep |> should equal "#t"
    "(rational? 1.5)" |> rep |> should equal "#t"
    "(rational? +inf.0)" |> rep |> should equal "#f"
    "(rational? 2.5)" |> rep |> should equal "#t"
    "(rational? +nan.0)" |> rep |> should equal "#f"
    "(rational? 2.5+0.0i)" |> rep |> should equal "#t"
    "(rational? 2.5+1.0i)" |> rep |> should equal "#f"

[<Fact>]
let ``integer?`` () =
    "(integer? 3.0)" |> rep |> should equal "#t"
    "(integer? 3.2)" |> rep |> should equal "#f"
    "(integer? 3.0+0.0i)" |> rep |> should equal "#t"
    "(integer? 1)" |> rep |> should equal "#t"
    "(integer? 1/2)" |> rep |> should equal "#f"
    "(integer? +inf.0)" |> rep |> should equal "#f"
    "(integer? +nan.0)" |> rep |> should equal "#f"

[<Fact>]
let ``exact?`` () =
    "(exact? 1)" |> rep |> should equal "#t"
    "(exact? 1.0)" |> rep |> should equal "#f"
    "(exact? 1/2)" |> rep |> should equal "#t"
    "(exact? 1+2i)" |> rep |> should equal "#f"
    "(exact? 0.5+0.75i)" |> rep |> should equal "#f"

[<Fact>]
let ``inexact?`` () =
    "(inexact? 1.0)" |> rep |> should equal "#t"
    "(inexact? 1)" |> rep |> should equal "#f"
    "(inexact? 1+2i)" |> rep |> should equal "#t"

[<Fact>]
let ``exact-integer?`` () =
    "(exact-integer? 1)" |> rep |> should equal "#t"
    "(exact-integer? 1.0)" |> rep |> should equal "#f"
    "(exact-integer? 3)" |> rep |> should equal "#t"
    "(exact-integer? 1/2)" |> rep |> should equal "#f"

[<Fact>]
let ``finite?`` () =
    "(finite? 3)" |> rep |> should equal "#t"
    "(finite? +inf.0)" |> rep |> should equal "#f"
    "(finite? 1.0)" |> rep |> should equal "#t"
    "(finite? +nan.0)" |> rep |> should equal "#f"
    "(finite? 1+2i)" |> rep |> should equal "#t"
    "(finite? \"a\")" |> rep |> should equal "#f"

[<Fact>]
let ``infinite?`` () =
    "(infinite? -inf.0)" |> rep |> should equal "#t"
    "(infinite? +inf.0)" |> rep |> should equal "#t"
    "(infinite? 1)" |> rep |> should equal "#f"
    "(infinite? 1+2i)" |> rep |> should equal "#f"
    "(infinite? +inf.0+1i)" |> rep |> should equal "#t"
    "(infinite? 1+inf.0i)" |> rep |> should equal "#t"

[<Fact>]
let ``nan?`` () =
    "(nan? +nan.0)" |> rep |> should equal "#t"
    "(nan? 1)" |> rep |> should equal "#f"
    "(nan? 1.0)" |> rep |> should equal "#f"
    "(nan? 1+2i)" |> rep |> should equal "#f"
    "(nan? +nan.0+1i)" |> rep |> should equal "#t"
    "(nan? 1+nan.0i)" |> rep |> should equal "#t"

[<Fact>]
let ``'='`` () =
    "(= 1 1)" |> rep |> should equal "#t"
    "(= 1 2)" |> rep |> should equal "#f"
    "(= 1 1 1)" |> rep |> should equal "#t"
    "(= 1 1 2)" |> rep |> should equal "#f"
    "(= 1 1.0)" |> rep |> should equal "#t"
    "(= 1.0 1.0)" |> rep |> should equal "#t"
    "(= 1/2 0.5)" |> rep |> should equal "#t"
    "(= 1+2i (+ 1 0+2i))" |> rep |> should equal "#t"
    "(= 1 1+0i)" |> rep |> should equal "#t"
    "(=)" |> rep |> should equal "#t"

[<Fact>]
let ``'<'`` () =
    "(< 1 2)" |> rep |> should equal "#t"
    "(< 2 1)" |> rep |> should equal "#f"
    "(< 1 2 3)" |> rep |> should equal "#t"
    "(< 1 3 2)" |> rep |> should equal "#f"
    "(< 1/2 1)" |> rep |> should equal "#t"
    "(< 1.0 2.0)" |> rep |> should equal "#t"
    "(< 1 1.5)" |> rep |> should equal "#t"
    "(< 1.5 2)" |> rep |> should equal "#t"
    "(< 1/2 0.6)" |> rep |> should equal "#t"
    "(< 1+0i 2+0i)" |> rep |> should equal "#t"

[<Fact>]
let ``'>'`` () =
    "(> 2 1)" |> rep |> should equal "#t"
    "(> 1 2)" |> rep |> should equal "#f"
    "(> 3 2 1)" |> rep |> should equal "#t"
    "(> 1.5 1)" |> rep |> should equal "#t"
    "(> 0.6 1/2)" |> rep |> should equal "#t"
    "(> 2+0i 1+0i)" |> rep |> should equal "#t"

[<Fact>]
let ``<=`` () =
    "(<= 1 1)" |> rep |> should equal "#t"
    "(<= 1 2)" |> rep |> should equal "#t"
    "(<= 2 1)" |> rep |> should equal "#f"
    "(<= 1/2 0.5)" |> rep |> should equal "#t"
    "(<= 1+0i 1+0i)" |> rep |> should equal "#t"

[<Fact>]
let ``>=`` () =
    "(>= 1 1)" |> rep |> should equal "#t"
    "(>= 2 1)" |> rep |> should equal "#t"
    "(>= 1 2)" |> rep |> should equal "#f"
    "(>= 0.5 1/2)" |> rep |> should equal "#t"
    "(>= 1+0i 1+0i)" |> rep |> should equal "#t"

[<Fact>]
let ``zero?`` () =
    "(zero? 0)" |> rep |> should equal "#t"
    "(zero? 1)" |> rep |> should equal "#f"
    "(zero? 0.0)" |> rep |> should equal "#t"
    "(zero? 0.0+0.0i)" |> rep |> should equal "#t"

[<Fact>]
let ``positive?`` () =
    "(positive? 1)" |> rep |> should equal "#t"
    "(positive? -1)" |> rep |> should equal "#f"
    "(positive? 0)" |> rep |> should equal "#f"
    "(positive? 1/2)" |> rep |> should equal "#t"
    "(positive? 1.5)" |> rep |> should equal "#t"

[<Fact>]
let ``negative?`` () =
    "(negative? -1)" |> rep |> should equal "#t"
    "(negative? 1)" |> rep |> should equal "#f"
    "(negative? 0)" |> rep |> should equal "#f"
    "(negative? -1/2)" |> rep |> should equal "#t"
    "(negative? -1.5)" |> rep |> should equal "#t"

[<Fact>]
let ``odd?`` () =
    "(odd? 3)" |> rep |> should equal "#t"
    "(odd? 3.0)" |> rep |> should equal "#t"
    "(odd? 1)" |> rep |> should equal "#t"
    "(odd? 2)" |> rep |> should equal "#f"
    "(odd? -1)" |> rep |> should equal "#t"
    "(odd? 3.0+0.0i)" |> rep |> should equal "#t"

[<Fact>]
let ``even?`` () =
    "(even? 2)" |> rep |> should equal "#t"
    "(even? 2.0)" |> rep |> should equal "#t"
    "(even? 0)" |> rep |> should equal "#t"
    "(even? 4)" |> rep |> should equal "#t"
    "(even? 3)" |> rep |> should equal "#f"
    "(even? 4.0+0.0i)" |> rep |> should equal "#t"

[<Fact>]
let max () =
    "(max 3 4)" |> rep |> should equal "4"
    "(max 3.9 4)" |> rep |> should equal "4"
    "(max 1 2.0)" |> rep |> should equal "2"
    "(max 1/2 0.6)" |> rep |> should equal "0.6"
    "(max 3 4.5 2)" |> rep |> should equal "4.5"

[<Fact>]
let min () =
    "(min 3 4)" |> rep |> should equal "3"
    "(min 3.9 4)" |> rep |> should equal "3.9"
    "(min 1 2.0)" |> rep |> should equal "1"
    "(min 3 4.5 2)" |> rep |> should equal "2"

[<Fact>]
let ``+`` () =
    "(+)" |> rep |> should equal "0"
    "(+ 10)" |> rep |> should equal "10"
    "(+ 10 2)" |> rep |> should equal "12"
    "(+ 10 2 3)" |> rep |> should equal "15"
    "(+ 1.5 2.5)" |> rep |> should equal "4"
    "(+ 1 1.0)" |> rep |> should equal "2"
    "(+ 1.0 1)" |> rep |> should equal "2"
    "(+ 1 1.5)" |> rep |> should equal "2.5"
    "(+ 1.5 1)" |> rep |> should equal "2.5"
    "(+ 1/2 0.5)" |> rep |> should equal "1"
    "(+ 1+2i 3+4i)" |> rep |> should equal "4+6i"
    "(+ 1+2i 3)" |> rep |> should equal "4+2i"
    "(+ 1 1+2i)" |> rep |> should equal "2+2i"
    "(+ 1+2i 0.5)" |> rep |> should equal "1.5+2i"
    "(+ 5)" |> rep |> should equal "5"
    "(+ 1.0)" |> rep |> should equal "1"
    "(+ 1+2i)" |> rep |> should equal "1+2i"

[<Fact>]
let ``*`` () =
    "(*)" |> rep |> should equal "1"
    "(* 10)" |> rep |> should equal "10"
    "(* 10 2)" |> rep |> should equal "20"
    "(* 10 2 3)" |> rep |> should equal "60"
    "(* 2 1.5)" |> rep |> should equal "3"
    "(* 1/2 2.0)" |> rep |> should equal "1"
    "(* 2 0+1i)" |> rep |> should equal "0+2i"
    "(* 1+2i 2)" |> rep |> should equal "2+4i"
    "(* 1+2i 0.5)" |> rep |> should equal "0.5+1i"
    "(* 5)" |> rep |> should equal "5"
    "(* 1.0)" |> rep |> should equal "1"
    "(* 1+2i)" |> rep |> should equal "1+2i"

[<Fact>]
let ``-`` () =
    "(-)" |> rep |> should equal "0"
    "(- 10)" |> rep |> should equal "-10"
    "(- 10 2)" |> rep |> should equal "8"
    "(- 10 2 3)" |> rep |> should equal "5"
    "(- 1 0.5)" |> rep |> should equal "0.5"
    "(- 3.0 1)" |> rep |> should equal "2"
    "(- 1+2i 1+2i)" |> rep |> should equal "0+0i"
    "(- 5)" |> rep |> should equal "-5"
    "(- 1.0)" |> rep |> should equal "-1"
    "(- 1+2i)" |> rep |> should equal "-1-2i"

[<Fact>]
let ``/`` () =
    "(/)" |> rep |> should equal "1"
    "(/ 10)" |> rep |> should equal "1/10"
    "(/ 9 2)" |> rep |> should equal "9/2"
    "(/ 12 2 3)" |> rep |> should equal "2"
    "(/ 3 4 5)" |> rep |> should equal "3/20"
    "(/ 3.0 2)" |> rep |> should equal "1.5"
    "(/ 1 2.0)" |> rep |> should equal "0.5"
    "(/ 1+2i 1+2i)" |> rep |> should equal "1+0i"
    "(/ 5)" |> rep |> should equal "1/5"
    "(/ 2.0)" |> rep |> should equal "0.5"
    "(/ 1 0)" |> rep |> should startWith "Division by zero."

[<Fact>]
let abs () =
    "(abs -7)" |> rep |> should equal "7"
    "(abs -3.14)" |> rep |> should equal "3.14"
    "(abs 3+4i)" |> rep |> should equal "5"
    "(abs 0+3i)" |> rep |> should equal "3"
    "(abs 0+0i)" |> rep |> should equal "0"

[<Fact>]
let ``floor/`` () =
    "(floor/ 10 3)" |> rep |> should equal "(values 3 1)"
    "(floor/ -10 3)" |> rep |> should equal "(values -4 2)"

[<Fact>]
let ``floor-quotient`` () =
    "(floor-quotient 10 3)" |> rep |> should equal "3"
    "(floor-quotient -10 3)" |> rep |> should equal "-4"

[<Fact>]
let ``floor-remainder`` () =
    "(floor-remainder 10 3)" |> rep |> should equal "1"
    "(floor-remainder -10 3)" |> rep |> should equal "2"

[<Fact>]
let ``truncate/`` () =
    "(truncate/ 10 3)" |> rep |> should equal "(values 3 1)"
    "(truncate/ -10 3)" |> rep |> should equal "(values -3 -1)"

[<Fact>]
let ``truncate-quotient`` () =
    "(truncate-quotient 10 3)" |> rep |> should equal "3"
    "(truncate-quotient -10 3)" |> rep |> should equal "-3"

[<Fact>]
let ``truncate-remainder`` () =
    "(truncate-remainder 10 3)" |> rep |> should equal "1"
    "(truncate-remainder -10 3)" |> rep |> should equal "-1"

[<Fact>]
let quotient () =
    "(quotient 10 3)" |> rep |> should equal "3"
    "(quotient -10 3)" |> rep |> should equal "-3"

[<Fact>]
let remainder () =
    "(remainder 10 3)" |> rep |> should equal "1"
    "(remainder -10 3)" |> rep |> should equal "-1"
    "(remainder 1 0)" |> rep |> should startWith "Division by zero."

[<Fact>]
let modulo () =
    "(modulo 10 3)" |> rep |> should equal "1"
    "(modulo -10 3)" |> rep |> should equal "2"

[<Fact>]
let gcd () =
    "(gcd 32 -36)" |> rep |> should equal "4"
    "(gcd 12 18 24)" |> rep |> should equal "6"
    "(gcd 5)" |> rep |> should equal "5"
    "(gcd)" |> rep |> should equal "0"

[<Fact>]
let lcm () =
    "(lcm 32 -36)" |> rep |> should equal "288"
    "(lcm 2 3 4)" |> rep |> should equal "12"
    "(lcm 5)" |> rep |> should equal "5"
    "(lcm)" |> rep |> should equal "1"

[<Fact>]
let numerator () =
    "(numerator 1/2)" |> rep |> should equal "1"
    "(numerator 3)" |> rep |> should equal "3"
    "(numerator 1.5)" |> rep |> should equal "3"
    "(numerator 1.0)" |> rep |> should equal "1"

[<Fact>]
let denominator () =
    "(denominator 1/2)" |> rep |> should equal "2"
    "(denominator 3)" |> rep |> should equal "1"
    "(denominator 1.5)" |> rep |> should equal "2"
    "(denominator 1.0)" |> rep |> should equal "1"

[<Fact>]
let floor () =
    "(floor 2.5)" |> rep |> should equal "2"
    "(floor -2.5)" |> rep |> should equal "-3"
    "(floor 5/2)" |> rep |> should equal "2"
    "(floor -5/2)" |> rep |> should equal "-3"

[<Fact>]
let ceiling () =
    "(ceiling 2.5)" |> rep |> should equal "3"
    "(ceiling -2.5)" |> rep |> should equal "-2"
    "(ceiling 5/2)" |> rep |> should equal "3"
    "(ceiling -5/2)" |> rep |> should equal "-2"

[<Fact>]
let truncate () =
    "(truncate 2.5)" |> rep |> should equal "2"
    "(truncate -2.5)" |> rep |> should equal "-2"
    "(truncate 5/2)" |> rep |> should equal "2"
    "(truncate -5/2)" |> rep |> should equal "-2"

[<Fact>]
let round () =
    "(round 1.5)" |> rep |> should equal "2"
    "(round 2.5)" |> rep |> should equal "2"
    "(round 3.5)" |> rep |> should equal "4"
    "(round 5/2)" |> rep |> should equal "2"
    "(round 7/2)" |> rep |> should equal "4"

[<Fact>]
let ``rationalize`` () =
    "(rationalize 0.3 1/10)" |> rep |> should equal "1/3"
    "(rationalize 1/2 1/10)" |> rep |> should equal "1/2"
    "(rationalize 1+2i 1/10)" |> rep |> should equal "1+2i"
    "(rationalize -0.3 1/10)" |> rep |> should equal "-1/3"
    "(rationalize 0.1 0.2)" |> rep |> should equal "0"
    "(rationalize +inf.0 0.1)" |> rep |> should equal "+inf.0"
    "(rationalize 1.0 0.1)" |> rep |> should equal "1"
    "(rationalize 1e20 1)" |> rep |> should not' (equal "0")

[<Fact>]
let exp () =
    "(exp 0)" |> rep |> should equal "1"
    "(exp 1.0)" |> rep |> should not' (equal "0")
    "(exp 1+2i)" |> rep |> should not' (equal "0")

[<Fact>]
let log () =
    "(log 1)" |> rep |> should equal "0"
    "(log 1.0)" |> rep |> should equal "0"
    "(log 8 2)" |> rep |> should equal "3"
    "(log 1/2)" |> rep |> should not' (equal "0")
    "(log 1+0i)" |> rep |> should equal "0+0i"

[<Fact>]
let sin () =
    "(sin 0)" |> rep |> should equal "0"
    "(sin 0.0)" |> rep |> should equal "0"
    "(sin 1/2)" |> rep |> should not' (equal "0")
    "(sin 1+2i)" |> rep |> should not' (equal "0")

[<Fact>]
let ``cos`` () =
    "(cos 0.0)" |> rep |> should equal "1"
    "(cos 1+2i)" |> rep |> should not' (equal "0")
    "(cos 1/2)" |> rep |> should not' (equal "0")

[<Fact>]
let ``tan`` () =
    "(tan 0.0)" |> rep |> should equal "0"
    "(tan 1+2i)" |> rep |> should not' (equal "0")
    "(tan 1/2)" |> rep |> should not' (equal "0")

[<Fact>]
let ``asin`` () =
    "(asin 1/2)" |> rep |> should not' (equal "0")
    "(asin 0.5)" |> rep |> should not' (equal "0")
    "(asin 1+2i)" |> rep |> should not' (equal "0")
    "(asin 2.0)" |> rep |> should not' (equal "+nan.0")

[<Fact>]
let ``acos`` () =
    "(acos 1/2)" |> rep |> should not' (equal "0")
    "(acos 0.5)" |> rep |> should not' (equal "0")
    "(acos 1+2i)" |> rep |> should not' (equal "0")
    "(acos 2.0)" |> rep |> should not' (equal "+nan.0")

[<Fact>]
let ``atan`` () =
    "(atan 1/2)" |> rep |> should not' (equal "0")
    "(atan 0.5)" |> rep |> should not' (equal "0")
    "(atan 1+2i)" |> rep |> should not' (equal "0")
    "(atan 1 1)" |> rep |> should not' (equal "0")
    "(atan 1/2 1/2)" |> rep |> should not' (equal "0")
    "(atan 1.0 1.0)" |> rep |> should not' (equal "0")

[<Fact>]
let square () =
    "(square 3)" |> rep |> should equal "9"
    "(square -3.0)" |> rep |> should equal "9"

[<Fact>]
let sqrt () =
    "(sqrt 4)" |> rep |> should equal "2"
    "(sqrt -1)" |> rep |> should equal "0+1i"
    "(sqrt -4.0)" |> rep |> should equal "0+2i"
    "(sqrt 1/4)" |> rep |> should equal "0.5"
    "(sqrt 3+4i)" |> rep |> should equal "2+1i"

[<Fact>]
let exactIntegerSqrt () =
    "(exact-integer-sqrt 4)" |> rep |> should equal "(values 2 0)"
    "(exact-integer-sqrt 5)" |> rep |> should equal "(values 2 1)"

[<Fact>]
let expt () =
    "(expt 2 3)" |> rep |> should equal "8"
    "(expt 4 0.5)" |> rep |> should equal "2+0i"
    "(expt 2 10)" |> rep |> should equal "1024"
    "(expt 5 -1)" |> rep |> should equal "1/5"
    "(expt -2 -3)" |> rep |> should equal "-1/8"

[<Fact>]
let ``make-rectangular`` () =
    "(make-rectangular 3 4)" |> rep |> should equal "3+4i"
    "(make-rectangular 0 0)" |> rep |> should equal "0+0i"
    "(make-rectangular 1/2 1/4)" |> rep |> should equal "0.5+0.25i"

[<Fact>]
let ``make-polar`` () =
    "(make-polar 1 0)" |> rep |> should equal "1+0i"

[<Fact>]
let ``real-part`` () =
    "(real-part 3+4i)" |> rep |> should equal "3"
    "(real-part 5)" |> rep |> should equal "5"
    "(real-part 2.5)" |> rep |> should equal "2.5"

[<Fact>]
let ``imag-part`` () =
    "(imag-part 3+4i)" |> rep |> should equal "4"
    "(imag-part 5)" |> rep |> should equal "0"
    "(imag-part 2.5)" |> rep |> should equal "0"

[<Fact>]
let ``magnitude`` () =
    "(magnitude 3+4i)" |> rep |> should equal "5"

[<Fact>]
let ``angle`` () =
    "(angle 1+0i)" |> rep |> should equal "0"

[<Fact>]
let inexact () =
    "(inexact 1)" |> rep |> should equal "1"
    "(inexact 1.0)" |> rep |> should equal "1"
    "(inexact 1+2i)" |> rep |> should equal "1+2i"

[<Fact>]
let exact () =
    "(exact 1.0)" |> rep |> should equal "1"
    "(exact 1)" |> rep |> should equal "1"
    "(exact 0.5)" |> rep |> should equal "1/2"

[<Fact>]
let ``number->string`` () =
    "(number->string 42)" |> rep |> should equal "\"42\""
    "(number->string 3.14)" |> rep |> should equal "\"3.14\""
    "(number->string 8 2)" |> rep |> should equal "\"1000\""
    "(number->string 8 8)" |> rep |> should equal "\"10\""
    "(number->string 42 10)" |> rep |> should equal "\"42\""
    "(number->string 3.14 10)" |> rep |> should equal "\"3.14\""
    "(number->string 255 16)" |> rep |> should equal "\"ff\""

[<Fact>]
let ``string->number`` () =
    "(string->number \"42\")" |> rep |> should equal "42"
    "(string->number \"3.14\")" |> rep |> should equal "3.14"
    "(string->number \"1000\" 2)" |> rep |> should equal "8"
    "(string->number \"10\" 8)" |> rep |> should equal "8"
    "(string->number \"42\" 10)" |> rep |> should equal "42"
    "(string->number \"ff\" 16)" |> rep |> should equal "255"
    "(string->number \"not-a-number\")" |> rep |> should equal "#f"

[<Fact>]
let ``math error paths`` () =
    "(zero? 1 2)" |> rep |> should startWith "'(1 2)' invalid zero? parameter"

    "(positive? 1 2)"
    |> rep
    |> should startWith "'(1 2)' invalid positive? parameter"

    "(negative? 1 2)"
    |> rep
    |> should startWith "'(1 2)' invalid negative? parameter"

    "(odd? 1 2)" |> rep |> should startWith "'(1 2)' invalid odd? parameter"

    "(even? 1 2)" |> rep |> should startWith "'(1 2)' invalid even? parameter"

    "(max)" |> rep |> should startWith "'()' invalid max parameter"

    "(min)" |> rep |> should startWith "'()' invalid min parameter"

    "(+ \"a\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(- \"a\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(* \"a\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(/ \"a\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(+ 1 \"a\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(+ 1 \"a\" 2)" |> rep |> should startWith "'\"a\"' is not a number"

    "(< 1+2i 3+4i)" |> rep |> should startWith "Ordering on complex numbers"

    "(floor/ 0)" |> rep |> should startWith "'(0)' invalid floor/ parameter"

    "(floor/ 1 0)" |> rep |> should startWith "Division by zero"

    "(truncate/ 1 0)" |> rep |> should startWith "Division by zero"

    "(quotient 1 0)" |> rep |> should startWith "Division by zero"

    "(modulo 1 0)" |> rep |> should startWith "Division by zero"

    "(gcd 1.5)" |> rep |> should startWith "'1.5' is not an integer in gcd"

    "(lcm 1.5)" |> rep |> should startWith "'1.5' is not an integer in lcm"

    "(floor \"a\")" |> rep |> should startWith "'\"a\"' invalid floor parameter"

    "(ceiling \"a\")" |> rep |> should startWith "'\"a\"' invalid ceiling parameter"

    "(truncate \"a\")"
    |> rep
    |> should startWith "'\"a\"' invalid truncate parameter"

    "(round \"a\")" |> rep |> should startWith "'\"a\"' invalid round parameter"

    "(abs)" |> rep |> should startWith "'()' invalid abs parameter"

    "(abs \"a\")" |> rep |> should startWith "'\"a\"' invalid abs parameter"

    "(expt 0 -1)" |> rep |> should startWith "Division by zero in expt"

    "(exact-integer-sqrt -1)"
    |> rep
    |> should startWith "'(-1)' invalid exact-integer-sqrt parameter"

    "(log \"a\")" |> rep |> should startWith "'(\"a\")' invalid log parameter"

    "(atan)" |> rep |> should startWith "'()' invalid atan parameter"

    "(atan 1 2 3)" |> rep |> should startWith "'(1 2 3)' invalid atan parameter"

    "(square)" |> rep |> should startWith "'()' invalid square parameter"

    "(make-rectangular)"
    |> rep
    |> should startWith "'()' invalid make-rectangular parameter"

    "(make-polar)" |> rep |> should startWith "'()' invalid make-polar parameter"

    "(numerator \"a\")"
    |> rep
    |> should startWith "'\"a\"' invalid numerator parameter"

    "(denominator \"a\")"
    |> rep
    |> should startWith "'\"a\"' invalid denominator parameter"

    "(inexact \"a\")" |> rep |> should startWith "'\"a\"' invalid inexact parameter"

    "(exact \"a\")" |> rep |> should startWith "'\"a\"' invalid exact parameter"

    "(number->string 10 3)"
    |> rep
    |> should startWith "'3' unsupported radix in number->string"

    "(rationalize)" |> rep |> should startWith "'()' invalid rationalize parameter"

    "(rationalize 1.0)"
    |> rep
    |> should startWith "'(1)' invalid rationalize parameter"

    "(rationalize \"a\" 1/10)" |> rep |> should startWith "'\"a\"' is not a number"

    "(rationalize 1/10 \"a\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(log)" |> rep |> should startWith "'()' invalid log parameter"

    "(expt)" |> rep |> should startWith "'()' invalid expt parameter"

    "(floor-quotient 1.5 2)"
    |> rep
    |> should startWith "'(1.5 2)' invalid floor-quotient parameter"

    "(floor-remainder 1.5 2)"
    |> rep
    |> should startWith "'(1.5 2)' invalid floor-remainder parameter"

    "(truncate/ 1.5 2)"
    |> rep
    |> should startWith "'(1.5 2)' invalid truncate/ parameter"

    "(truncate-quotient 1.5 2)"
    |> rep
    |> should startWith "'(1.5 2)' invalid truncate-quotient parameter"

    "(truncate-remainder 1.5 2)"
    |> rep
    |> should startWith "'(1.5 2)' invalid truncate-remainder parameter"

    "(quotient 1.5 2)"
    |> rep
    |> should startWith "'(1.5 2)' invalid quotient parameter"

    "(remainder 1.5 2)"
    |> rep
    |> should startWith "'(1.5 2)' invalid remainder parameter"

    "(modulo 1.5 2)" |> rep |> should startWith "'(1.5 2)' invalid modulo parameter"

    "(number->string 42 10 3)"
    |> rep
    |> should startWith "'(42 10 3)' invalid number->string parameter"

    "(max 1+2i 3+4i)" |> rep |> should startWith "Ordering on complex numbers"

    "(min 1+2i 3+4i)" |> rep |> should startWith "Ordering on complex numbers"

    "(make-rectangular 1 \"a\")"
    |> rep
    |> should startWith "'\"a\"' is not a number"

    "(make-polar 1 \"a\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(< \"a\" \"b\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(= \"a\" 1)" |> rep |> should startWith "'\"a\"' is not a number"

    "(< \"a\" 1)" |> rep |> should startWith "'\"a\"' is not a number"

    "(> \"a\" 1)" |> rep |> should startWith "'\"a\"' is not a number"

    "(<= \"a\" 1)" |> rep |> should startWith "'\"a\"' is not a number"

    "(>= \"a\" 1)" |> rep |> should startWith "'\"a\"' is not a number"

    "(max \"a\" 1)" |> rep |> should startWith "'\"a\"' is not a number"

    "(min \"a\" 1)" |> rep |> should startWith "'\"a\"' is not a number"

    "(= 1 \"a\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(< 1 \"a\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(> 1 \"a\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(<= 1 \"a\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(>= 1 \"a\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(max 1 \"a\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(min 1 \"a\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(real-part \"a\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(imag-part \"a\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(magnitude \"a\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(angle \"a\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(/ 1 \"a\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(number->string 1/2 10)" |> rep |> should equal "\"1/2\""

    "(log 1 \"a\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(expt 1 \"a\")" |> rep |> should startWith "'\"a\"' is not a number"

    "(atan \"a\" 1)" |> rep |> should startWith "atan expected real"

    "(number? 1 2)" |> rep |> should startWith "'(1 2)' invalid number? parameter"

    "(complex? 1 2)" |> rep |> should startWith "'(1 2)' invalid complex? parameter"

    "(real? 1 2)" |> rep |> should startWith "'(1 2)' invalid real? parameter"

    "(rational? 1 2)"
    |> rep
    |> should startWith "'(1 2)' invalid rational? parameter"

    "(integer? 1 2)" |> rep |> should startWith "'(1 2)' invalid integer? parameter"

    "(exact? 1 2)" |> rep |> should startWith "'(1 2)' invalid exact? parameter"

    "(inexact? 1 2)" |> rep |> should startWith "'(1 2)' invalid inexact? parameter"

    "(exact-integer? 1 2)"
    |> rep
    |> should startWith "'(1 2)' invalid exact-integer? parameter"

    "(finite? 1 2)" |> rep |> should startWith "'(1 2)' invalid finite? parameter"

    "(infinite? 1 2)"
    |> rep
    |> should startWith "'(1 2)' invalid infinite? parameter"

    "(nan? 1 2)" |> rep |> should startWith "'(1 2)' invalid nan? parameter"

    "(string->number \"abc\" 3)" |> rep |> should equal "#f"
