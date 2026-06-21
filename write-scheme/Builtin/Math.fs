namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Math =
    let bothOk (a: Result<'a, _>) (b: Result<'b, _>) =
        match a, b with
        | Ok a, Ok b -> Ok(a, b)
        | Error e, _ -> Error e
        | _, Error e -> Error e

    let isNumber context pos cont =
        wrapUnaryPred
            "number?"
            (function
            | SRational _
            | SReal _
            | SComplex _ -> true
            | _ -> false)
            context
            pos
            cont

    let isComplex context pos cont =
        wrapUnaryPred
            "complex?"
            (function
            | SRational _
            | SReal _
            | SComplex _ -> true
            | _ -> false)
            context
            pos
            cont

    let isReal context pos cont =
        wrapUnaryPred
            "real?"
            (function
            | SRational _
            | SReal _ -> true
            | SComplex c when c.Imaginary = 0.0 -> true
            | _ -> false)
            context
            pos
            cont

    let isRational context pos cont =
        wrapUnarySExprPred "rational?" (SNumber.tryGetFiniteRealValue >> Option.isSome) context pos cont

    let isInteger context pos cont =
        wrapUnarySExprPred "integer?" (SNumber.tryGetExactIntegerValue >> Option.isSome) context pos cont

    let isExact context pos cont =
        wrapUnaryPred
            "exact?"
            (function
            | SRational _ -> true
            | _ -> false)
            context
            pos
            cont

    let isInexact context pos cont =
        wrapUnaryPred
            "inexact?"
            (function
            | SReal _
            | SComplex _ -> true
            | _ -> false)
            context
            pos
            cont

    let isExactInteger context pos cont =
        wrapUnaryPred
            "exact-integer?"
            (function
            | SRational(_, d) when d = 1I -> true
            | _ -> false)
            context
            pos
            cont

    let isFinite context pos cont =
        wrapUnaryPred
            "finite?"
            (function
            | SRational _ -> true
            | SReal r when SNumber.finiteFloat r -> true
            | SComplex c when SNumber.finiteFloat c.Real && SNumber.finiteFloat c.Imaginary -> true
            | _ -> false)
            context
            pos
            cont

    let isInfinite context pos cont =
        wrapUnaryPred
            "infinite?"
            (function
            | SReal r when System.Double.IsInfinity r -> true
            | SComplex c when System.Double.IsInfinity c.Real || System.Double.IsInfinity c.Imaginary -> true
            | _ -> false)
            context
            pos
            cont

    let isNaN context pos cont =
        wrapUnaryPred
            "nan?"
            (function
            | SReal r when System.Double.IsNaN r -> true
            | SComplex c when System.Double.IsNaN c.Real || System.Double.IsNaN c.Imaginary -> true
            | _ -> false)
            context
            pos
            cont

    let toComplex =
        function
        | SRational(n, d), _ -> System.Numerics.Complex(SNumber.toFloat n d, 0.0) |> Ok
        | SReal r, _ -> System.Numerics.Complex(r, 0.0) |> Ok
        | SComplex c, _ -> Ok c
        | x -> x |> invalid (snd x) "'%s' is not a number."

    let comparePred pred1 pred2 pred3 =
        function
        | (SRational(n1, d1), _), (SRational(n2, d2), _) -> pred1 (n1 * d2) (n2 * d1) |> Ok
        | (SReal r1, _), (SReal r2, _) -> pred2 r1 r2 |> Ok
        | (x, p1), (y, p2) ->
            match toComplex (x, p1), toComplex (y, p2) with
            | Ok c1, Ok c2 -> pred3 c1 c2
            | Error e, _
            | _, Error e -> Error e

    [<TailCall>]
    let rec compare pos pred1 pred2 pred3 n =
        function
        | [] -> Ok(STrue, pos)
        | x :: xs ->
            match comparePred pred1 pred2 pred3 (n, x) with
            | Ok true -> compare pos pred1 pred2 pred3 x xs
            | Ok false -> Ok(SFalse, pos)
            | Error e -> Error e

    let compareNumber pred1 pred2 pred3 pos cont =
        function
        | [] -> Ok(STrue, pos) |> cont
        | x :: xs -> xs |> compare pos pred1 pred2 pred3 x |> cont

    let complexReal op (x: System.Numerics.Complex) (y: System.Numerics.Complex) =
        if x.Imaginary = 0.0 && y.Imaginary = 0.0 then
            op x.Real y.Real |> Ok
        else
            EvalError("Ordering on complex numbers with non-zero imaginary parts is undefined.", None)
            |> Error

    let sEqualNumber context =
        compareNumber (=) (=) (fun x y -> Ok(x = y))

    let sLessNumber context = compareNumber (<) (<) (complexReal (<))

    let sGreaterNumber context = compareNumber (>) (>) (complexReal (>))

    let sLessEqualNumber context =
        compareNumber (<=) (<=) (complexReal (<=))

    let sGreaterEqualNumber context =
        compareNumber (>=) (>=) (complexReal (>=))

    let isZero context pos cont =
        function
        | [ x ] -> sEqualNumber context pos cont [ x; SZero, pos ]
        | x -> x |> invalidParameter pos "'%s' invalid zero? parameter." |> cont

    let isPositive context pos cont =
        function
        | [ x ] -> sGreaterNumber context pos cont [ x; SZero, pos ]
        | x -> x |> invalidParameter pos "'%s' invalid positive? parameter." |> cont

    let isNegative context pos cont =
        function
        | [ x ] -> sLessNumber context pos cont [ x; SZero, pos ]
        | x -> x |> invalidParameter pos "'%s' invalid negative? parameter." |> cont

    let isOdd context pos cont =
        function
        | [ x ] ->
            Ok(
                x
                |> SNumber.tryGetExactIntegerValue
                |> Option.map (fun n -> abs n % 2I = 1I)
                |> Option.defaultValue false
                |> toSBool,
                pos
            )
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid odd? parameter." |> cont

    let isEven context pos cont =
        function
        | [ x ] ->
            Ok(
                x
                |> SNumber.tryGetExactIntegerValue
                |> Option.map (fun n -> n % 2I = 0I)
                |> Option.defaultValue false
                |> toSBool,
                pos
            )
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid even? parameter." |> cont

    let isAnyInexact =
        List.exists (function
            | SRational _, _ -> false
            | _ -> true)

    [<TailCall>]
    let rec findExtremumValue pred1 pred2 pred3 acc =
        function
        | [] -> Ok acc
        | x :: xs ->
            match comparePred pred1 pred2 pred3 (acc, x) with
            | Ok true -> xs |> findExtremumValue pred1 pred2 pred3 acc
            | Ok false -> xs |> findExtremumValue pred1 pred2 pred3 x
            | Error e -> Error e

    let extremum pred1 pred2 pred3 name context pos cont args =
        if args |> List.isEmpty then
            args
            |> invalidParameter pos (sprintf "'%%s' invalid %s parameter." name)
            |> cont
        else
            let h = args.Head

            args.Tail
            |> findExtremumValue pred1 pred2 pred3 h
            |> Result.map (fun v ->
                if isAnyInexact args then
                    match v with
                    | SRational(n, d), _ -> SReal(float n / float d), pos
                    | x -> x
                else
                    v)
            |> cont

    let sMax context =
        extremum (>) (>) (complexReal (>)) "max" context

    let sMin context =
        extremum (<) (<) (complexReal (<)) "min" context

    [<TailCall>]
    let rec loopCalc op pos cont acc =
        function
        | [] ->
            match acc with
            | Ok a -> a |> SNumber.toSExpr pos |> cont
            | Error e -> Error e |> cont
        | (x, elPos) :: xs ->
            match acc with
            | Error e -> Error e |> cont
            | Ok a ->
                match SNumber.ofExpr (x, elPos) with
                | Error _ ->
                    let msg = sprintf "'%s' is not a number." ((x, elPos) |> Print.print)
                    EvalError(msg, pos) |> Error |> cont
                | Ok b ->
                    match op a b with
                    | Error msg -> EvalError(msg, pos) |> Error |> cont
                    | Ok res -> xs |> loopCalc op pos cont (Ok res)

    let calc op identity pos cont args =
        match args with
        | [] -> identity |> SNumber.toSExpr pos |> cont
        | (x, elPos) :: xs ->
            match SNumber.ofExpr (x, elPos) with
            | Error _ ->
                let msg = sprintf "'%s' is not a number." ((x, elPos) |> Print.print)
                EvalError(msg, pos) |> Error |> cont
            | Ok a ->
                match xs with
                | [] ->
                    match op identity a with
                    | Error msg -> EvalError(msg, pos) |> Error |> cont
                    | Ok res -> SNumber.toSExpr pos res |> cont
                | _ -> xs |> loopCalc op pos cont (Ok a)

    let sAddNumber context = calc SNumber.add (NRational(0I, 1I))
    let sMultiplyNumber context = calc SNumber.mul (NRational(1I, 1I))
    let sSubtractNumber context = calc SNumber.sub (NRational(0I, 1I))
    let sDivideNumber context = calc SNumber.div (NRational(1I, 1I))

    let sAbs context pos cont =
        function
        | [ x ] ->
            match SNumber.ofExpr x with
            | Ok n ->
                match SNumber.abs n with
                | Ok result -> SNumber.toSExpr pos result |> cont
                | Error msg -> EvalError(msg, pos) |> Error |> cont
            | Error _ -> x |> invalid pos "'%s' invalid abs parameter." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid abs parameter." |> cont

    let truncateDiv n d = n / d, n % d

    let floorDiv (n: bigint) (d: bigint) =
        let quotient, remainder = truncateDiv n d

        if remainder <> 0I && n.Sign <> d.Sign then
            quotient - 1I, remainder + d
        else
            quotient, remainder

    let checkedBigintDivOp name divFn context pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            if n2 = 0I then
                EvalError("Division by zero.", pos) |> Error |> cont
            else
                let q, r = divFn n1 n2
                Ok(SValues [ newInteger q, pos; newInteger r, pos ], pos) |> cont
        | x -> x |> invalidParameter pos (sprintf "'%%s' invalid %s parameter." name) |> cont

    let sFloorDiv context =
        checkedBigintDivOp "floor/" floorDiv context

    let sFloorQuotient context pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            let quotient, _ = floorDiv n1 n2
            Ok(newInteger quotient, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid floor-quotient parameter." |> cont

    let sFloorRemainder context pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            let _, remainder = floorDiv n1 n2
            Ok(newInteger remainder, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid floor-remainder parameter." |> cont

    let sTruncateDiv context =
        checkedBigintDivOp "truncate/" truncateDiv context

    let sTruncateQuotient context pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            let quotient, _ = truncateDiv n1 n2
            Ok(newInteger quotient, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid truncate-quotient parameter." |> cont

    let sTruncateRemainder context pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            let _, remainder = truncateDiv n1 n2
            Ok(newInteger remainder, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid truncate-remainder parameter." |> cont

    let checkNonZeroDivisor n1 n2 pos =
        if n2 = 0I then
            EvalError("Division by zero.", pos) |> Error
        else
            Ok(n1, n2)

    let sQuotient context pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            checkNonZeroDivisor n1 n2 pos
            |> Result.map (fun (n1', n2') -> n1' / n2' |> newInteger, pos)
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid quotient parameter." |> cont

    let sRemainder context pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            checkNonZeroDivisor n1 n2 pos
            |> Result.map (fun (n1', n2') -> n1' % n2' |> newInteger, pos)
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid remainder parameter." |> cont

    let modulo (n1: bigint) (n2: bigint) =
        let remainder = n1 % n2

        if remainder <> 0I && n1.Sign <> n2.Sign then
            remainder + n2
        else
            remainder

    let sModulo context pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            checkNonZeroDivisor n1 n2 pos
            |> Result.map (fun (n1', n2') -> modulo n1' n2' |> newInteger, pos)
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid modulo parameter." |> cont

    let gcd x y = bigint.GreatestCommonDivisor(x, y)

    let sGcd context pos cont =
        mapResult (function
            | SRational(n, d), _ when d = 1I -> Ok n
            | x -> x |> invalid (snd x) "'%s' is not an integer in gcd.")
        >> Result.map (List.fold gcd 0I >> fun v -> newInteger v, pos)
        >> cont

    let lcm x y =
        if x = 0I || y = 0I then 0I else abs (x * y) / gcd x y

    let sLcm context pos cont =
        mapResult (function
            | SRational(n, d), _ when d = 1I -> Ok n
            | x -> x |> invalid (snd x) "'%s' is not an integer in lcm.")
        >> Result.map (List.fold lcm 1I >> fun v -> newInteger v, pos)
        >> cont

    let rationalProjection name projectField defaultResult context pos cont =
        wrapUnary
            name
            (function
            | SRational(n, d), _ -> newInteger (projectField n d) |> Ok
            | SReal r, _ when SNumber.finiteFloat r ->
                match realToRational r with
                | SRational(n, d) -> SReal(float (projectField n d))
                | _ -> defaultResult r
                |> Ok
            | x -> x |> invalid (snd x) (sprintf "'%%s' invalid %s parameter." name))
            context
            pos
            cont

    let sNumerator context =
        rationalProjection "numerator" (fun n _ -> n) SReal context

    let sDenominator context =
        rationalProjection "denominator" (fun _ d -> d) (fun _ -> SReal 1.0) context

    let roundingOp name adjust floatRound context pos cont =
        wrapUnary
            name
            (function
            | SRational(n, d), _ -> adjust n d |> newInteger |> Ok
            | SReal r, _ -> floatRound r |> SReal |> Ok
            | x -> x |> invalid (snd x) (sprintf "'%%s' invalid %s parameter." name))
            context
            pos
            cont

    let sFloor context =
        roundingOp
            "floor"
            (fun n d ->
                let q, r = truncateDiv n d
                if r <> 0I && n.Sign <> d.Sign then q - 1I else q)
            floor
            context

    let sCeiling context =
        roundingOp
            "ceiling"
            (fun n d ->
                let q, r = truncateDiv n d
                if r <> 0I && n.Sign = d.Sign then q + 1I else q)
            ceil
            context

    let sTruncate context =
        roundingOp "truncate" (fun n d -> n / d) truncate context

    let sRound context =
        roundingOp "round" (fun n d -> float n / float d |> round |> bigint) round context

    [<TailCall>]
    let rec simplestRational n1 d1 n2 d2 next =
        let floorL = if n1 >= 0I then n1 / d1 else (n1 - d1 + 1I) / d1
        let ceilL = if n1 % d1 = 0I then floorL else floorL + 1I
        let floorR = if n2 >= 0I then n2 / d2 else (n2 - d2 + 1I) / d2

        if ceilL <= floorR then
            if ceilL > 0I then ceilL, 1I
            elif floorR < 0I then floorR, 1I
            else 0I, 1I
            |> Ok
            |> next
        else
            simplestRational d2 (n2 - floorL * d2) d1 (n1 - floorL * d1) (fun result ->
                match result with
                | Ok(pn, pd) -> normalizeRational (floorL * pn + pd) pn |> next
                | Error e -> Error e |> next)

    let sRationalize context pos cont =
        let toExactValue =
            function
            | SReal r, _ when SNumber.finiteFloat r -> (realToRational r, pos) |> Ok
            | x -> Ok x

        function
        | [ x; y ] ->
            bothOk (toExactValue x) (toExactValue y)
            |> Result.bind (fun (xVal, yVal) ->
                bothOk (sSubtractNumber context pos id [ xVal; yVal ]) (sAddNumber context pos id [ xVal; yVal ]))
            |> Result.bind (fun (l, r) ->
                match l, r with
                | (SRational(l1, l2), _), (SRational(r1, r2), _) ->
                    simplestRational l1 l2 r1 r2 id
                    |> Result.map (fun (n, d) -> (if d = 1I then newInteger n else SRational(n, d)), pos)
                    |> Result.mapError (fun msg -> EvalError(msg, pos))
                | _ -> Ok x)
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid rationalize parameter." |> cont

    let sExp context pos cont =
        SNumber.unaryMath "exp" (exp >> SReal) (System.Numerics.Complex.Exp >> SComplex) context pos cont

    let sLog context pos cont =
        function
        | [ _ ] as args ->
            SNumber.unaryMath "log" (log >> SReal) (System.Numerics.Complex.Log >> SComplex) context pos cont args
        | [ x; y ] ->
            match bothOk (toComplex x) (toComplex y) with
            | Ok(c1, c2) ->
                let res = System.Numerics.Complex.Log c1 / System.Numerics.Complex.Log c2
                Ok((if res.Imaginary = 0.0 then SReal res.Real else SComplex res), pos) |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid log parameter." |> cont

    let sSin context pos cont =
        SNumber.unaryMath "sin" (sin >> SReal) (System.Numerics.Complex.Sin >> SComplex) context pos cont

    let sCos context pos cont =
        SNumber.unaryMath "cos" (cos >> SReal) (System.Numerics.Complex.Cos >> SComplex) context pos cont

    let sTan context pos cont =
        SNumber.unaryMath "tan" (tan >> SReal) (System.Numerics.Complex.Tan >> SComplex) context pos cont

    let sAsin context pos cont =
        SNumber.unaryMathDomain
            "asin"
            (fun r -> r >= -1.0 && r <= 1.0)
            (asin >> SReal)
            (System.Numerics.Complex.Asin >> SComplex)
            context
            pos
            cont

    let sAcos context pos cont =
        SNumber.unaryMathDomain
            "acos"
            (fun r -> r >= -1.0 && r <= 1.0)
            (acos >> SReal)
            (System.Numerics.Complex.Acos >> SComplex)
            context
            pos
            cont

    let sAtan context pos cont =
        function
        | [ _ ] as args ->
            SNumber.unaryMath "atan" (atan >> SReal) (System.Numerics.Complex.Atan >> SComplex) context pos cont args
        | [ y; x ] ->
            let toFloat =
                function
                | SReal r, _ -> Ok r
                | SRational(n, d), _ -> float n / float d |> Ok
                | _, pos -> EvalError("atan expected real", pos) |> Error

            match bothOk (toFloat y) (toFloat x) with
            | Ok(yVal, xVal) -> Ok(System.Math.Atan2(yVal, xVal) |> SReal, pos) |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid atan parameter." |> cont

    let sSquare context pos cont =
        function
        | [ x ] -> sMultiplyNumber context pos cont [ x; x ]
        | x -> x |> invalidParameter pos "'%s' invalid square parameter." |> cont

    let sSqrt context pos cont =
        SNumber.unaryMathDomain
            "sqrt"
            (fun r -> r >= 0.0)
            (sqrt >> SReal)
            (System.Numerics.Complex.Sqrt >> SComplex)
            context
            pos
            cont

    [<TailCall>]
    let rec bigintSqrt low high n =
        if low + 1I >= high then
            low
        else
            let mid = (low + high) / 2I

            if mid * mid <= n then
                bigintSqrt mid high n
            else
                bigintSqrt low mid n

    let sExactIntegerSqrt context pos cont =
        function
        | [ SRational(n, d), _ ] when d = 1I && n >= 0I ->
            let sqrt = if n < 2I then n else bigintSqrt 1I (n + 1I) n
            let remainder = n - sqrt * sqrt
            Ok(SValues [ newInteger sqrt, pos; newInteger remainder, pos ], pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid exact-integer-sqrt parameter." |> cont

    let sExptInteger n1 n2 pos =
        if n2 >= 0I then
            Ok(bigint.Pow(n1, int n2) |> newInteger, pos)
        else if n1 = 0I then
            EvalError("Division by zero in expt.", pos) |> Error
        else
            newSRational 1I (bigint.Pow(n1, int -n2))
            |> Result.map (fun r -> r, pos)
            |> Result.mapError (fun msg -> EvalError(msg, pos))

    let sExptComplex x y =
        bothOk (toComplex x) (toComplex y)
        |> Result.map (fun (c1, c2) -> System.Numerics.Complex.Pow(c1, c2) |> SComplex)

    let sExpt context pos cont =
        function
        | [ x; y ] ->
            match x, y with
            | (SRational(n1, d1), _), (SRational(n2, d2), _) when d1 = 1I && d2 = 1I -> sExptInteger n1 n2 pos |> cont
            | _ -> sExptComplex x y |> Result.map (fun r -> r, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid expt parameter." |> cont

    let sMakeRectangular context pos cont =
        function
        | [ real; image ] ->
            match bothOk (toComplex real) (toComplex image) with
            | Ok(cr, ci) -> Ok(System.Numerics.Complex(cr.Real, ci.Real) |> SComplex, pos) |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-rectangular parameter." |> cont

    let sMakePolar context pos cont =
        function
        | [ magnitude; angle ] ->
            match bothOk (toComplex magnitude) (toComplex angle) with
            | Ok(cm, ca) ->
                Ok(System.Numerics.Complex.FromPolarCoordinates(cm.Real, ca.Real) |> SComplex, pos)
                |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-polar parameter." |> cont

    let sRealPart context pos cont =
        wrapUnary "real-part" (toComplex >> Result.map (fun c -> SReal c.Real)) context pos cont

    let sImagPart context pos cont =
        wrapUnary "imag-part" (toComplex >> Result.map (fun c -> SReal c.Imaginary)) context pos cont

    let sMagnitude context pos cont =
        wrapUnary "magnitude" (toComplex >> Result.map (fun c -> SReal c.Magnitude)) context pos cont

    let sAngle context pos cont =
        wrapUnary "angle" (toComplex >> Result.map (fun c -> SReal c.Phase)) context pos cont

    let sInexact context pos cont =
        wrapUnary
            "inexact"
            (function
            | SRational(n, d), _ -> SReal(float n / float d) |> Ok
            | SReal _, _ as x -> fst x |> Ok
            | SComplex _, _ as x -> fst x |> Ok
            | x -> x |> invalid (snd x) "'%s' invalid inexact parameter.")
            context
            pos
            cont

    let sExact context pos cont =
        wrapUnary
            "exact"
            (function
            | SRational _, _ as x -> fst x |> Ok
            | SReal r, _ when SNumber.finiteFloat r -> realToRational r |> Ok
            | x -> x |> invalid (snd x) "'%s' invalid exact parameter.")
            context
            pos
            cont

    let radixToString pos radix n =
        match int radix with
        | 2 -> System.Convert.ToString(int64 n, 2) |> Ok
        | 8 -> System.Convert.ToString(int64 n, 8) |> Ok
        | 10 -> string n |> Ok
        | 16 -> System.Convert.ToString(int64 n, 16) |> Ok
        | x -> EvalError(sprintf "'%d' unsupported radix in number->string." x, pos) |> Error

    let sNumberToString context pos cont =
        function
        | [ n ] -> Ok(n |> Print.print |> newSString true, pos) |> cont
        | [ n; SRational(radix, d), _ ] when d = 1I ->
            match n with
            | SRational(n', d'), _ when d' = 1I -> radixToString pos radix n'
            | SRational(n', d'), _ -> Ok(sprintf "%A/%A" n' d')
            | _ -> Ok(n |> Print.print)
            |> Result.map (fun s -> s |> newSString true, pos)
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid number->string parameter." |> cont

    let radixPrefix =
        function
        | 2 -> Some "#b"
        | 8 -> Some "#o"
        | 10 -> Some "#d"
        | 16 -> Some "#x"
        | _ -> None

    let sStringToNumber context pos cont =
        let handleResult =
            function
            | Ok(SRational _, _)
            | Ok(SReal _, _)
            | Ok(SComplex _, _) as n -> n |> cont
            | _ -> Ok(SFalse, pos) |> cont

        function
        | [ SString s, _ ] -> s.runes |> runesToString |> Read.read false |> handleResult
        | [ SString data, _; SRational(radix, d), _ ] when d = 1I ->
            match radixPrefix (int radix) with
            | Some prefix -> prefix + (data.runes |> runesToString) |> Read.read false |> handleResult
            | None -> (SFalse, pos) |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string->number parameter." |> cont
