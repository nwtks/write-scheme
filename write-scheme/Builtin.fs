namespace WriteScheme

open Type
open Eval
open WriteScheme.Builtins

module Builtin =
    let builtin =
        extendEnvs
            []
            [ "quote", SSyntax sQuote |> ref
              "lambda", SSyntax sLambda |> ref
              "if", SSyntax sIf |> ref
              "set!", SSyntax sSet |> ref
              "cond", SSyntax sCond |> ref
              "and", SSyntax sAnd |> ref
              "or", SSyntax sOr |> ref
              "when", SSyntax sWhen |> ref
              "unless", SSyntax sUnless |> ref
              "let", SSyntax sLet |> ref
              "let*", SSyntax sLetStar |> ref
              "letrec", SSyntax sLetRec |> ref
              "letrec*", SSyntax sLetRecStar |> ref
              "begin", SSyntax sBegin |> ref
              "quasiquote", SSyntax sQuasiquote |> ref
              "define", SSyntax sDefine |> ref
              "define-syntax", SSyntax sDefineSyntax |> ref
              "syntax-rules", SSyntax sSyntaxRules |> ref
              "eqv?", SProcedure isEqv |> ref
              "eq?", SProcedure isEqv |> ref
              "equal?", SProcedure isEqual |> ref
              "number?", SProcedure isNumber |> ref
              "=", SProcedure equalNumber |> ref
              "<", SProcedure lessNumber |> ref
              ">", SProcedure greaterNumber |> ref
              "<=", SProcedure lessEqualNumber |> ref
              ">=", SProcedure greaterEqualNumber |> ref
              "zero?", SProcedure isZero |> ref
              "positive?", SProcedure isPositive |> ref
              "negative?", SProcedure isNegative |> ref
              "+", SProcedure addNumber |> ref
              "*", SProcedure multiplyNumber |> ref
              "-", SProcedure subtractNumber |> ref
              "/", SProcedure divideNumber |> ref
              "not", SProcedure sNot |> ref
              "boolean?", SProcedure isBoolean |> ref
              "pair?", SProcedure isPair |> ref
              "cons", SProcedure sCons |> ref
              "car", SProcedure sCar |> ref
              "cdr", SProcedure sCdr |> ref
              "null?", SProcedure isNull |> ref
              "list?", SProcedure isList |> ref
              "list", SProcedure sList |> ref
              "append", SProcedure sAppend |> ref
              "symbol?", SProcedure isSymbol |> ref
              "char?", SProcedure isChar |> ref
              "string?", SProcedure isString |> ref
              "procedure?", SProcedure isProcedure |> ref
              "apply", SProcedure sApply |> ref
              "map", SProcedure sMap |> ref
              "for-each", SProcedure sForEach |> ref
              "call/cc", SProcedure sCallCC |> ref
              "call-with-current-continuation", SProcedure sCallCC |> ref
              "display", SProcedure sDisplay |> ref
              "load", SProcedure sLoad |> ref
              "raise", SProcedure sRaise |> ref
              "with-exception-handler", SProcedure sWithExceptionHandler |> ref
              "error", SProcedure sError |> ref
              "error-object?", SProcedure isErrorObject |> ref
              "error-object-message", SProcedure sErrorObjectMessage |> ref
              "error-object-irritants", SProcedure sErrorObjectIrritants |> ref ]
