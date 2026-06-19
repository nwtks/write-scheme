module WriteScheme.Tests.ByteVectorTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext

[<Fact>]
let ``bytevector?`` () =
    "(bytevector? #u8(1 2 3))" |> rep |> should equal "#t"
    "(bytevector? '#u8(1 2 3))" |> rep |> should equal "#t"
    "(bytevector? #u8())" |> rep |> should equal "#t"
    "(bytevector? '#u8())" |> rep |> should equal "#t"
    "(bytevector? '())" |> rep |> should equal "#f"
    "(bytevector? 1)" |> rep |> should equal "#f"

    "(bytevector? 1 2)"
    |> rep
    |> should startWith "'(1 2)' invalid bytevector? parameter"

[<Fact>]
let ``make-bytevector`` () =
    "(bytevector-length (make-bytevector 5))" |> rep |> should equal "5"
    "(make-bytevector 3 0)" |> rep |> should equal "#u8(0 0 0)"
    "(make-bytevector 0)" |> rep |> should equal "#u8()"
    "(make-bytevector 3 255)" |> rep |> should equal "#u8(255 255 255)"

    "(make-bytevector -1)"
    |> rep
    |> should startWith "'(-1)' invalid make-bytevector parameter"

    "(make-bytevector 5 256)"
    |> rep
    |> should startWith "'(5 256)' invalid make-bytevector parameter"

    "(make-bytevector 5 -1)"
    |> rep
    |> should startWith "'(5 -1)' invalid make-bytevector parameter"

[<Fact>]
let ``bytevector`` () =
    "(bytevector 1 2 3)" |> rep |> should equal "#u8(1 2 3)"
    "(bytevector)" |> rep |> should equal "#u8()"
    "(bytevector 256)" |> rep |> should startWith "'256' invalid bytevector element"

[<Fact>]
let ``bytevector-length`` () =
    "(bytevector-length #u8(1 2 3))" |> rep |> should equal "3"
    "(bytevector-length #u8())" |> rep |> should equal "0"

    "(bytevector-length '())"
    |> rep
    |> should startWith "'(())' invalid bytevector-length parameter"

[<Fact>]
let ``bytevector-u8-ref`` () =
    "(bytevector-u8-ref #u8(10 20 30) 0)" |> rep |> should equal "10"
    "(bytevector-u8-ref #u8(10 20 30) 2)" |> rep |> should equal "30"

    "(bytevector-u8-ref #u8(1 2 3) 3)"
    |> rep
    |> should startWith "'(#u8(1 2 3) 3)' invalid bytevector-u8-ref parameter"

    "(bytevector-u8-ref #u8(1 2 3) -1)"
    |> rep
    |> should startWith "'(#u8(1 2 3) -1)' invalid bytevector-u8-ref parameter"

[<Fact>]
let ``bytevector-u8-set!`` () =
    "(let ((v (bytevector 1 2 3))) (bytevector-u8-set! v 0 10) v)"
    |> rep
    |> should equal "#u8(10 2 3)"

    "(let ((v (bytevector 1 2 3))) (bytevector-u8-set! v 2 255) v)"
    |> rep
    |> should equal "#u8(1 2 255)"

    "(bytevector-u8-set! #u8(1 2 3) 3 10)"
    |> rep
    |> should startWith "'(#u8(1 2 3) 3 10)' invalid bytevector-u8-set! parameter"

    "(bytevector-u8-set! #u8(1 2 3) 0 256)"
    |> rep
    |> should startWith "'(#u8(1 2 3) 0 256)' invalid bytevector-u8-set! parameter"

[<Fact>]
let ``bytevector-copy`` () =
    "(let* ((a (bytevector 1 2 3)) (b (bytevector-copy a))) (bytevector-u8-set! a 0 10) b)"
    |> rep
    |> should equal "#u8(1 2 3)"

    "(bytevector-copy #u8(1 2 3 4 5) 2)" |> rep |> should equal "#u8(3 4 5)"
    "(bytevector-copy #u8(1 2 3 4 5) 2 4)" |> rep |> should equal "#u8(3 4)"

    "(bytevector-copy #u8(1 2 3) 5)"
    |> rep
    |> should startWith "'(#u8(1 2 3) 5)' invalid bytevector-copy parameter"

    "(bytevector-copy #u8(1 2 3) -1)"
    |> rep
    |> should startWith "'(#u8(1 2 3) -1)' invalid bytevector-copy parameter"

[<Fact>]
let ``bytevector-copy!`` () =
    "(let ((a (bytevector 1 2 3 4 5)) (b (bytevector 10 20 30))) (bytevector-copy! a 1 b) a)"
    |> rep
    |> should equal "#u8(1 10 20 30 5)"

    "(let ((a (bytevector 1 2 3 4 5)) (b (bytevector 10 20 30))) (bytevector-copy! a 1 b 1) a)"
    |> rep
    |> should equal "#u8(1 20 30 4 5)"

    "(let ((a (bytevector 1 2 3 4 5)) (b (bytevector 10 20 30))) (bytevector-copy! a 1 b 0 2) a)"
    |> rep
    |> should equal "#u8(1 10 20 4 5)"

    "(let ((v (bytevector 1 2 3 4 5))) (bytevector-copy! v 1 v 0 3) v)"
    |> rep
    |> should equal "#u8(1 1 2 3 5)"

    "(let ((v (bytevector 1 2 3 4 5))) (bytevector-copy! v 0 v 1 5) v)"
    |> rep
    |> should equal "#u8(2 3 4 5 5)"

    "(bytevector-copy! #u8(1 2) 3 #u8(10 20 30))"
    |> rep
    |> should startWith "'(#u8(1 2) 3 #u8(10 20 30))' invalid bytevector-copy! parameter"

    "(bytevector-copy! 1 0 #u8(10 20 30))"
    |> rep
    |> should startWith "'(1 0 #u8(10 20 30))' invalid bytevector-copy! parameter"

[<Fact>]
let ``bytevector-append`` () =
    "(bytevector-append #u8(1 2) #u8(3 4 5) #u8(6))"
    |> rep
    |> should equal "#u8(1 2 3 4 5 6)"

    "(bytevector-append)" |> rep |> should equal "#u8()"

    "(bytevector-append #u8(1) '(2))"
    |> rep
    |> should startWith "'(2)' is not a bytevector in bytevector-append"

[<Fact>]
let ``utf8->string`` () =
    "(utf8->string #u8(65 66 67))" |> rep |> should equal "\"ABC\""
    "(utf8->string #u8(#x41 #x42 #x43))" |> rep |> should equal "\"ABC\""
    "(utf8->string #u8(#x41 #x42 #x43) 1)" |> rep |> should equal "\"BC\""
    "(utf8->string #u8(#x41 #x42 #x43) 1 2)" |> rep |> should equal "\"B\""
    "(utf8->string #u8(240 159 141 142))" |> rep |> should equal "\"🍎\""

    "(utf8->string #u8(65 66) 5)"
    |> rep
    |> should startWith "'(#u8(65 66) 5)' invalid utf8->string parameter"

    "(utf8->string 1)"
    |> rep
    |> should startWith "'(1)' invalid utf8->string parameter"

[<Fact>]
let ``string->utf8`` () =
    "(string->utf8 \"ABC\")" |> rep |> should equal "#u8(65 66 67)"
    "(string->utf8 \"ABC\" 1)" |> rep |> should equal "#u8(66 67)"
    "(string->utf8 \"ABC\" 1 2)" |> rep |> should equal "#u8(66)"
    "(string->utf8 \"🍎\")" |> rep |> should equal "#u8(240 159 141 142)"
    "(string->utf8 \"a🍎b\" 1 2)" |> rep |> should equal "#u8(240 159 141 142)"

    "(string->utf8 \"AB\" 5)"
    |> rep
    |> should startWith "'(\"AB\" 5)' invalid string->utf8 parameter"

    "(string->utf8 1)"
    |> rep
    |> should startWith "'(1)' invalid string->utf8 parameter"
