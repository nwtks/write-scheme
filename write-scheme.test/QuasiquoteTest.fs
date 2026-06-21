module WriteScheme.Tests.QuasiquoteTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext

[<Fact>]
let ``quasiquote`` () =
    "`(list ,(+ 1 2) 4)" |> rep |> should equal "(list 3 4)"
    "`(a . ,(+ 1 2))" |> rep |> should equal "(a . 3)"
    "(let ((name 'a)) `(list ,name ',name))" |> rep |> should equal "(list a 'a)"

    "`((foo ,(- 10 3)) ,@(cdr '(c d)) . ,(car '(cons)))"
    |> rep
    |> should equal "((foo 7) d . cons)"

    "(let ((foo '(foo bar)) (baz 'baz)) `(list ,@foo ,baz))"
    |> rep
    |> should equal "(list foo bar baz)"

    "`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)"
    |> rep
    |> should equal "(a `(b ,(+ 1 2) ,(foo 4 d) e) f)"

    "(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))"
    |> rep
    |> should equal "(a `(b ,x ,'y d) e)"

    "`1" |> rep |> should equal "1"
    "`\"abc\"" |> rep |> should equal "\"abc\""
    "`#t" |> rep |> should equal "#t"
    "`a" |> rep |> should equal "a"
    "`()" |> rep |> should equal "()"

    "`(,@'(1 2) ,@'(3 4))" |> rep |> should equal "(1 2 3 4)"
    "`(,@'())" |> rep |> should equal "()"
    "`(,@'() 1 2)" |> rep |> should equal "(1 2)"

    "`(,@'() . ,@'())"
    |> rep
    |> should startWith "unquote-splicing must be in a list or vector context."

    "`,@'(1 2)"
    |> rep
    |> should startWith "unquote-splicing must be in a list or vector context."

    "``(a . ,,@'(1 2))"
    |> rep
    |> should startWith "unquote-splicing must be in a list or vector context."

    "`(,@'(1 2) . 3)" |> rep |> should equal "(1 2 . 3)"
    "`(1 2 . ,(append '(3 4) 5))" |> rep |> should equal "(1 2 3 4 . 5)"
    "`(,@'(1 2) . ,(append '(3 4) 5))" |> rep |> should equal "(1 2 3 4 . 5)"

    "`(a . ,(values 1 2))"
    |> rep
    |> should startWith "Multiple values in single value context."

    "`(,@(values '(1 2)))" |> rep |> should equal "(1 2)"
    "`#()" |> rep |> should equal "#()"
    "`#(1 ,@(values '(2 3)) 4)" |> rep |> should equal "#(1 2 3 4)"

    "(let ((x 1)) `#(a ,x c))" |> rep |> should equal "#(a 1 c)"
    "(let ((x '(1 2))) `#(a ,@x c))" |> rep |> should equal "#(a 1 2 c)"
    "(let ((x '(1 2))) `#(a ,@x ,x))" |> rep |> should equal "#(a 1 2 (1 2))"
    "(let ((x '(1 2))) `(a . ,x))" |> rep |> should equal "(a 1 2)"
    "(let ((x '(1 2))) `(a ,@x . 3))" |> rep |> should equal "(a 1 2 . 3)"
    "(let ((x '(1 2))) `(a `(b . ,@x)))" |> rep |> should equal "(a `(b . ,@x))"
    "(let ((x '(1 2))) `(a `(b ,@,x)))" |> rep |> should equal "(a `(b ,@(1 2)))"

    "(let ((x '(1 2)) (y '(3 4))) `(,@x . ,@y))"
    |> rep
    |> should startWith "unquote-splicing must be in a list or vector context."

    "(let ((x '((1 2)))) `(a `(b ,,@x)))"
    |> rep
    |> should startWith "unquote-splicing must be in a list or vector context."

[<Fact>]
let ``quasiquote quote keyword`` () =
    "'(quote a)" |> rep |> should equal "(quote a)"
