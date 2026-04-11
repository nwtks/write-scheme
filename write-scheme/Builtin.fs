namespace WriteScheme

open Type
open WriteScheme.Builtins

module Builtin =
    let builtin =
        Eval.extendEnvs
            []
            [ "quote", SSyntax sQuote |> ref
              "lambda", SSyntax sLambda |> ref
              "if", SSyntax sIf |> ref
              "set!", SSyntax sSet |> ref
              "cond", SSyntax sCond |> ref
              "case", SSyntax sCase |> ref
              "and", SSyntax sAnd |> ref
              "or", SSyntax sOr |> ref
              "when", SSyntax sWhen |> ref
              "unless", SSyntax sUnless |> ref
              "let", SSyntax sLet |> ref
              "let*", SSyntax sLetStar |> ref
              "letrec", SSyntax sLetRec |> ref
              "letrec*", SSyntax sLetRecStar |> ref
              "let-values", SSyntax sLetValues |> ref
              "let*-values", SSyntax sLetStarValues |> ref
              "begin", SSyntax sBegin |> ref
              "do", SSyntax sDo |> ref
              "delay", SSyntax sDelay |> ref
              "delay-force", SSyntax sDelayForce |> ref
              "parameterize", SSyntax sParameterize |> ref
              "guard", SSyntax sGuard |> ref
              "let-syntax", SSyntax sLetSyntax |> ref
              "letrec-syntax", SSyntax sLetRecSyntax |> ref
              "quasiquote", SSyntax sQuasiquote |> ref
              "syntax-rules", SSyntax sSyntaxRules |> ref
              "syntax-error", SSyntax sSyntaxError |> ref
              "define", SSyntax sDefine |> ref
              "define-values", SSyntax sDefineValues |> ref
              "define-syntax", SSyntax sDefineSyntax |> ref
              "define-record-type", SSyntax sDefineRecordType |> ref
              "force", SProcedure sForce |> ref
              "promise?", SProcedure isPromise |> ref
              "make-promise", SProcedure sMakePromise |> ref
              "make-parameter", SProcedure sMakeParameter |> ref
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
              "vector?", SProcedure isVector |> ref
              "make-vector", SProcedure sMakeVector |> ref
              "vector", SProcedure sVector |> ref
              "vector-length", SProcedure sVectorLength |> ref
              "vector-ref", SProcedure sVectorRef |> ref
              "vector-set!", SProcedure sVectorSet |> ref
              "vector->list", SProcedure sVectorToList |> ref
              "list->vector", SProcedure sListToVector |> ref
              "vector-fill!", SProcedure sVectorFill |> ref
              "procedure?", SProcedure isProcedure |> ref
              "apply", SProcedure sApply |> ref
              "map", SProcedure sMap |> ref
              "for-each", SProcedure sForEach |> ref
              "call-with-current-continuation", SProcedure sCallCC |> ref
              "call/cc", SProcedure sCallCC |> ref
              "values", SProcedure sValues |> ref
              "call-with-values", SProcedure sCallWithValues |> ref
              "dynamic-wind", SProcedure sDynamicWind |> ref
              "with-exception-handler", SProcedure sWithExceptionHandler |> ref
              "raise", SProcedure sRaise |> ref
              "error", SProcedure sError |> ref
              "error-object?", SProcedure isErrorObject |> ref
              "error-object-message", SProcedure sErrorObjectMessage |> ref
              "error-object-irritants", SProcedure sErrorObjectIrritants |> ref
              "display", SProcedure sDisplay |> ref
              "load", SProcedure sLoad |> ref ]
