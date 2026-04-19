module WriteScheme.Tests.ByteVectorTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtin

[<Fact>]
let ``bytevector?`` () =
    "(bytevector? #u8(1 2 3))" |> rep |> should equal "#t"
    "(bytevector? '#u8(1 2 3))" |> rep |> should equal "#t"
    "(bytevector? #u8())" |> rep |> should equal "#t"
    "(bytevector? '#u8())" |> rep |> should equal "#t"
    "(bytevector? '())" |> rep |> should equal "#f"
    "(bytevector? 1)" |> rep |> should equal "#f"

[<Fact>]
let ``make-bytevector`` () =
    "(bytevector-length (make-bytevector 5))" |> rep |> should equal "5"
    "(make-bytevector 3 0)" |> rep |> should equal "#u8(0 0 0)"
    "(make-bytevector 0)" |> rep |> should equal "#u8()"
    "(make-bytevector 3 255)" |> rep |> should equal "#u8(255 255 255)"

[<Fact>]
let ``bytevector`` () =
    "(bytevector 1 2 3)" |> rep |> should equal "#u8(1 2 3)"
    "(bytevector)" |> rep |> should equal "#u8()"

[<Fact>]
let ``bytevector-length`` () =
    "(bytevector-length #u8(1 2 3))" |> rep |> should equal "3"
    "(bytevector-length #u8())" |> rep |> should equal "0"

[<Fact>]
let ``bytevector-u8-ref`` () =
    "(bytevector-u8-ref #u8(10 20 30) 0)" |> rep |> should equal "10"
    "(bytevector-u8-ref #u8(10 20 30) 2)" |> rep |> should equal "30"

[<Fact>]
let ``bytevector-u8-set!`` () =
    "(let ((v (bytevector 1 2 3))) (bytevector-u8-set! v 0 10) v)"
    |> rep
    |> should equal "#u8(10 2 3)"

    "(let ((v (bytevector 1 2 3))) (bytevector-u8-set! v 2 255) v)"
    |> rep
    |> should equal "#u8(1 2 255)"

[<Fact>]
let ``bytevector-copy`` () =
    "(let* ((a (bytevector 1 2 3)) (b (bytevector-copy a))) (bytevector-u8-set! a 0 10) b)"
    |> rep
    |> should equal "#u8(1 2 3)"

    "(bytevector-copy #u8(1 2 3 4 5) 2)" |> rep |> should equal "#u8(3 4 5)"
    "(bytevector-copy #u8(1 2 3 4 5) 2 4)" |> rep |> should equal "#u8(3 4)"

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

[<Fact>]
let ``bytevector-append`` () =
    "(bytevector-append #u8(1 2) #u8(3 4 5) #u8(6))"
    |> rep
    |> should equal "#u8(1 2 3 4 5 6)"

    "(bytevector-append)" |> rep |> should equal "#u8()"

[<Fact>]
let ``utf8->string`` () =
    "(utf8->string #u8(65 66 67))" |> rep |> should equal "\"ABC\""
    "(utf8->string #u8(#x41 #x42 #x43))" |> rep |> should equal "\"ABC\""
    "(utf8->string #u8(#x41 #x42 #x43) 1)" |> rep |> should equal "\"BC\""
    "(utf8->string #u8(#x41 #x42 #x43) 1 2)" |> rep |> should equal "\"B\""

[<Fact>]
let ``string->utf8`` () =
    "(string->utf8 \"ABC\")" |> rep |> should equal "#u8(65 66 67)"
    "(string->utf8 \"ABC\" 1)" |> rep |> should equal "#u8(66 67)"
    "(string->utf8 \"ABC\" 1 2)" |> rep |> should equal "#u8(66)"
    "(string->utf8 \"🍎\")" |> rep |> should equal "#u8(240 159 141 142)"
    "(string->utf8 \"a🍎b\" 1 2)" |> rep |> should equal "#u8(240 159 141 142)"
