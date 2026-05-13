namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Math =
    let isNumber context pos cont =
        function
        | [ SRational _, _ ]
        | [ SReal _, _ ]
        | [ SComplex _, _ ] -> Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid number? parameter." |> cont

    let isComplex context pos cont =
        function
        | [ SComplex _, _ ]
        | [ SReal _, _ ]
        | [ SRational _, _ ] -> Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid complex? parameter." |> cont

    let isReal context pos cont =
        function
        | [ SRational _, _ ]
        | [ SReal _, _ ] -> Ok(STrue, pos) |> cont
        | [ SComplex c, _ ] when c.Imaginary = 0.0 -> Ok(STrue, pos) |> cont
        | [ SComplex _, _ ] -> Ok(SFalse, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid real? parameter." |> cont

    let finiteFloat d =
        not (System.Double.IsInfinity d || System.Double.IsNaN d)

    let noFractionFloat (d: float) = d = System.Math.Truncate d

    let isRational context pos cont =
        function
        | [ SRational _, _ ] -> Ok(STrue, pos) |> cont
        | [ SReal r, _ ] when finiteFloat r -> Ok(STrue, pos) |> cont
        | [ SComplex c, _ ] when c.Imaginary = 0.0 && finiteFloat c.Real -> Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid rational? parameter." |> cont

    let isInteger context pos cont =
        function
        | [ SRational(_, d), _ ] when d = 1I -> Ok(STrue, pos) |> cont
        | [ SReal r, _ ] when finiteFloat r && noFractionFloat r -> Ok(STrue, pos) |> cont
        | [ SComplex c, _ ] when c.Imaginary = 0.0 && finiteFloat c.Real && noFractionFloat c.Real ->
            Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid integer? parameter." |> cont

    let isExact context pos cont =
        function
        | [ SRational _, _ ] -> Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid exact? parameter." |> cont

    let isInexact context pos cont =
        function
        | [ SReal _, _ ]
        | [ SComplex _, _ ] -> Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid inexact? parameter." |> cont

    let isExactInteger context pos cont =
        function
        | [ SRational(_, d), _ ] when d = 1I -> Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid exact-integer? parameter." |> cont

    let isFinite context pos cont =
        function
        | [ SRational _, _ ] -> Ok(STrue, pos) |> cont
        | [ SReal r, _ ] -> Ok(finiteFloat r |> toSBool, pos) |> cont
        | [ SComplex c, _ ] -> Ok((finiteFloat c.Real && finiteFloat c.Imaginary) |> toSBool, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid finite? parameter." |> cont

    let isInfinite context pos cont =
        function
        | [ SReal r, _ ] -> Ok(System.Double.IsInfinity r |> toSBool, pos) |> cont
        | [ SComplex c, _ ] ->
            Ok(
                ((System.Double.IsInfinity c.Real || System.Double.IsInfinity c.Imaginary)
                 |> toSBool,
                 pos)
            )
            |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid infinite? parameter." |> cont

    let isNaN context pos cont =
        function
        | [ SReal r, _ ] -> Ok(System.Double.IsNaN r |> toSBool, pos) |> cont
        | [ SComplex c, _ ] ->
            Ok((System.Double.IsNaN c.Real || System.Double.IsNaN c.Imaginary) |> toSBool, pos)
            |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid nan? parameter." |> cont

    let toFloat x y = float x / float y

    let toComplex =
        function
        | SRational(n, d), _ -> System.Numerics.Complex(toFloat n d, 0.0) |> Ok
        | SReal r, _ -> System.Numerics.Complex(r, 0.0) |> Ok
        | SComplex c, _ -> Ok c
        | x -> x |> invalid (snd x) "'%s' is not a number."

    let comparePred pred1 pred2 pred3 =
        function
        | (SRational(n1, d1), _), (SRational(n2, d2), _) -> pred1 (n1 * d2) (n2 * d1) |> Ok
        | (SRational(n1, d1), _), (SReal r2, _) -> pred2 (toFloat n1 d1) r2 |> Ok
        | (SReal r1, _), (SRational(n2, d2), _) -> pred2 r1 (toFloat n2 d2) |> Ok
        | (SReal r1, _), (SReal r2, _) -> pred2 r1 r2 |> Ok
        | (SComplex _, _), _
        | _, (SComplex _, _) as pair ->
            match toComplex (fst pair), toComplex (snd pair) with
            | Ok c1, Ok c2 -> pred3 c1 c2
            | Error e, _ -> Error e
            | _, Error e -> Error e
        | _ -> Ok false

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
        | [ SRational(n, d), _ ] when d = 1I -> Ok(abs n % 2I = 1I |> toSBool, pos) |> cont
        | [ SReal r, _ ] when finiteFloat r && noFractionFloat r -> Ok(abs r % 2.0 = 1.0 |> toSBool, pos) |> cont
        | [ SComplex c, _ ] when c.Imaginary = 0.0 && finiteFloat c.Real && noFractionFloat c.Real ->
            Ok(abs c.Real % 2.0 = 1.0 |> toSBool, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid odd? parameter." |> cont

    let isEven context pos cont =
        function
        | [ SRational(n, d), _ ] when d = 1I -> Ok(n % 2I = 0I |> toSBool, pos) |> cont
        | [ SReal r, _ ] when finiteFloat r && noFractionFloat r -> Ok(r % 2.0 = 0.0 |> toSBool, pos) |> cont
        | [ SComplex c, _ ] when c.Imaginary = 0.0 && finiteFloat c.Real && noFractionFloat c.Real ->
            Ok(c.Real % 2.0 = 0.0 |> toSBool, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid even? parameter." |> cont

    let isAnyInexact =
        List.exists (function
            | SRational _, _ -> false
            | _ -> true)

    [<TailCall>]
    let rec findMaxValue acc =
        function
        | [] -> Ok acc
        | x :: xs ->
            match comparePred (>) (>) (complexReal (>)) (acc, x) with
            | Ok true -> xs |> findMaxValue acc
            | Ok false -> xs |> findMaxValue x
            | Error e -> Error e

    let sMax context pos cont args =
        if args |> List.isEmpty then
            args |> invalidParameter pos "'%s' invalid max parameter." |> cont
        else
            match args with
            | h :: t ->
                t
                |> findMaxValue h
                |> Result.map (fun maxVal ->
                    if isAnyInexact args then
                        match maxVal with
                        | SRational(n, d), _ -> float n / float d |> SReal, pos
                        | x -> x
                    else
                        maxVal)
                |> cont
            | [] -> args |> invalidParameter pos "'%s' invalid max parameter." |> cont

    [<TailCall>]
    let rec findMinValue acc =
        function
        | [] -> Ok acc
        | x :: xs ->
            match comparePred (<) (<) (complexReal (<)) (acc, x) with
            | Ok true -> xs |> findMinValue acc
            | Ok false -> xs |> findMinValue x
            | Error e -> Error e

    let sMin context pos cont args =
        if args |> List.isEmpty then
            args |> invalidParameter pos "'%s' invalid min parameter." |> cont
        else
            match args with
            | h :: t ->
                t
                |> findMinValue h
                |> Result.map (fun minVal ->
                    if isAnyInexact args then
                        match minVal with
                        | SRational(n, d), _ -> SReal(float n / float d), pos
                        | x -> x
                    else
                        minVal)
                |> cont
            | [] -> args |> invalidParameter pos "'%s' invalid min parameter." |> cont

    [<TailCall>]
    let rec loopCalc op1 op2 op3 pos cont acc =
        function
        | [] -> acc |> cont
        | (x, pos') :: xs ->
            let wrap res =
                res
                |> Result.map (fun n -> n, pos)
                |> Result.mapError (fun msg -> EvalError(msg, pos))

            match acc, x with
            | Ok(SRational(n1, d1), _), SRational(n2, d2) ->
                xs |> loopCalc op1 op2 op3 pos cont (op1 n1 d1 n2 d2 |> wrap)
            | Ok(SRational(n1, d1), _), SReal r2 -> xs |> loopCalc op1 op2 op3 pos cont (op2 (toFloat n1 d1) r2 |> wrap)
            | Ok(SReal r1, _), SRational(n2, d2) -> xs |> loopCalc op1 op2 op3 pos cont (op2 r1 (toFloat n2 d2) |> wrap)
            | Ok(SReal r1, _), SReal r2 -> xs |> loopCalc op1 op2 op3 pos cont (op2 r1 r2 |> wrap)
            | Ok(SComplex _, _), _
            | Ok _, SComplex _ ->
                match acc |> Result.bind toComplex, toComplex (x, pos') with
                | Ok c1, Ok c2 -> xs |> loopCalc op1 op2 op3 pos cont (op3 c1 c2 |> wrap)
                | Error e, _ -> Error e |> cont
                | _, Error e -> Error e |> cont
            | Ok a, b ->
                EvalError(sprintf "'%s', '%s' not number." (a |> Print.print) ((b, pos') |> Print.print), pos)
                |> Error
                |> cont
            | x, _ -> x |> cont

    let calc op1 op2 op3 ident1 ident2 ident3 pos cont =
        let wrap res =
            res
            |> Result.map (fun n -> n, pos)
            |> Result.mapError (fun msg -> EvalError(msg, pos))

        function
        | [] -> Ok(newInteger ident1, pos) |> cont
        | [ SRational(n, d), _ ] -> op1 ident1 1I n d |> wrap |> cont
        | [ SReal r, _ ] -> op2 ident2 r |> wrap |> cont
        | [ SComplex c, _ ] -> op3 ident3 c |> wrap |> cont
        | x :: xs -> xs |> loopCalc op1 op2 op3 pos cont (Ok x)

    let sAddNumber context =
        calc
            (fun n1 d1 n2 d2 -> newSRational (n1 * d2 + n2 * d1) (d1 * d2))
            (fun r1 r2 -> r1 + r2 |> SReal |> Ok)
            (fun c1 c2 -> c1 + c2 |> SComplex |> Ok)
            0I
            0.0
            System.Numerics.Complex.Zero

    let sMultiplyNumber context =
        calc
            (fun n1 d1 n2 d2 -> newSRational (n1 * n2) (d1 * d2))
            (fun r1 r2 -> r1 * r2 |> SReal |> Ok)
            (fun c1 c2 -> c1 * c2 |> SComplex |> Ok)
            1I
            1.0
            System.Numerics.Complex.One

    let sSubtractNumber context =
        calc
            (fun n1 d1 n2 d2 -> newSRational (n1 * d2 - n2 * d1) (d1 * d2))
            (fun r1 r2 -> r1 - r2 |> SReal |> Ok)
            (fun c1 c2 -> c1 - c2 |> SComplex |> Ok)
            0I
            0.0
            System.Numerics.Complex.Zero

    let sDivideNumber context =
        calc
            (fun n1 d1 n2 d2 -> newSRational (n1 * d2) (d1 * n2))
            (fun r1 r2 ->
                if r2 = 0.0 then
                    Error "Division by zero."
                else
                    r1 / r2 |> SReal |> Ok)
            (fun c1 c2 ->
                if c2.Magnitude = 0.0 then
                    Error "Division by zero."
                else
                    c1 / c2 |> SComplex |> Ok)
            1I
            1.0
            System.Numerics.Complex.One

    let sAbs context pos cont =
        function
        | [ SRational(n, d), _ ] ->
            newSRational (abs n) d
            |> Result.map (fun n -> n, pos)
            |> Result.mapError (fun msg -> EvalError(msg, pos))
            |> cont
        | [ SReal r, _ ] -> Ok(abs r |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> Ok(c.Magnitude |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid abs parameter." |> cont

    let truncateDiv n d = n / d, n % d

    let floorDiv (n: bigint) (d: bigint) =
        let quotient, remainder = truncateDiv n d

        if remainder <> 0I && n.Sign <> d.Sign then
            quotient - 1I, remainder + d
        else
            quotient, remainder

    let sFloorDiv context pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            if n2 = 0I then
                EvalError("Division by zero.", pos) |> Error |> cont
            else
                let quotient, remainder = floorDiv n1 n2
                Ok(SValues [ newInteger quotient, pos; newInteger remainder, pos ], pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid floor/ parameter." |> cont

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

    let sTruncateDiv context pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            if n2 = 0I then
                EvalError("Division by zero.", pos) |> Error |> cont
            else
                let quotient, remainder = truncateDiv n1 n2
                Ok(SValues [ newInteger quotient, pos; newInteger remainder, pos ], pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid truncate/ parameter." |> cont

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

    let sQuotient context pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            if n2 = 0I then
                EvalError("Division by zero.", pos) |> Error |> cont
            else
                Ok(n1 / n2 |> newInteger, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid quotient parameter." |> cont

    let sRemainder context pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            if n2 = 0I then
                EvalError("Division by zero.", pos) |> Error |> cont
            else
                Ok(n1 % n2 |> newInteger, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid remainder parameter." |> cont

    let sModulo context pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            if n2 = 0I then
                EvalError("Division by zero.", pos) |> Error |> cont
            else
                let remainder = n1 % n2

                if remainder <> 0I && n1.Sign <> n2.Sign then
                    Ok(remainder + n2 |> newInteger, pos) |> cont
                else
                    Ok(remainder |> newInteger, pos) |> cont
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

    let sNumerator context pos cont =
        function
        | [ SRational(n, _), _ ] -> Ok(newInteger n, pos) |> cont
        | [ SReal r, _ ] when finiteFloat r ->
            match realToRational r with
            | SRational(n, _) -> Ok(float n |> SReal, pos) |> cont
            | _ -> Ok(SReal r, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid numerator parameter." |> cont

    let sDenominator context pos cont =
        function
        | [ SRational(_, d), _ ] -> Ok(newInteger d, pos) |> cont
        | [ SReal r, _ ] when finiteFloat r ->
            match realToRational r with
            | SRational(_, d) -> Ok(float d |> SReal, pos) |> cont
            | _ -> Ok(SReal 1.0, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid denominator parameter." |> cont

    let sFloor context pos cont =
        function
        | [ SRational(n, d), _ ] ->
            let quotient, remainder = truncateDiv n d

            if remainder <> 0I && n.Sign <> d.Sign then
                quotient - 1I
            else
                quotient
            |> fun v -> Ok(newInteger v, pos) |> cont
        | [ SReal r, _ ] -> Ok(r |> floor |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid floor parameter." |> cont

    let sCeiling context pos cont =
        function
        | [ SRational(n, d), _ ] ->
            let quotient, remainder = truncateDiv n d

            if remainder <> 0I && n.Sign = d.Sign then
                quotient + 1I
            else
                quotient
            |> fun v -> Ok(newInteger v, pos) |> cont
        | [ SReal r, _ ] -> Ok(r |> ceil |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid ceiling parameter." |> cont

    let sTruncate context pos cont =
        function
        | [ SRational(n, d), _ ] -> Ok(n / d |> newInteger, pos) |> cont
        | [ SReal r, _ ] -> Ok(r |> truncate |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid truncate parameter." |> cont

    let sRound context pos cont =
        function
        | [ SRational(n, d), _ ] -> Ok(float n / float d |> round |> bigint |> newInteger, pos) |> cont
        | [ SReal r, _ ] -> Ok(r |> round |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid round parameter." |> cont

    [<TailCall>]
    let rec simplestRational pos cont =
        function
        | Ok(SRational(n1, d1), _), Ok(SRational(n2, d2), _) ->
            let floorL = if n1 >= 0I then n1 / d1 else (n1 - d1 + 1I) / d1
            let ceilL = if n1 % d1 = 0I then floorL else floorL + 1I
            let floorR = if n2 >= 0I then n2 / d2 else (n2 - d2 + 1I) / d2

            if ceilL <= floorR then
                if ceilL > 0I then Ok(newInteger ceilL, pos) |> cont
                elif floorR < 0I then Ok(newInteger floorR, pos) |> cont
                else Ok(SZero, pos) |> cont
            else
                simplestRational
                    pos
                    (function
                    | Ok(SRational(pn, pd), _) ->
                        newSRational (floorL * pn + pd) pn
                        |> Result.map (fun n -> n, pos)
                        |> Result.mapError (fun msg -> EvalError(msg, pos))
                        |> cont
                    | x -> x |> cont)
                    (newSRational d2 (n2 - floorL * d2)
                     |> Result.map (fun n -> n, pos)
                     |> Result.mapError (fun msg -> EvalError(msg, pos)),
                     newSRational d1 (n1 - floorL * d1)
                     |> Result.map (fun n -> n, pos)
                     |> Result.mapError (fun msg -> EvalError(msg, pos)))
        | _ -> Error(EvalError("Operands not rational", pos)) |> cont

    let sRationalize context pos cont =
        let toExactValue =
            function
            | SRational _, _ as x -> Ok x
            | SReal r, _ when finiteFloat r -> Ok(realToRational r, pos)
            | x -> Ok x

        function
        | [ x; y ] ->
            match toExactValue x, toExactValue y with
            | Ok(SRational _, _ as xVal), Ok(SRational _, _ as yVal) ->
                match sSubtractNumber context pos id [ xVal; yVal ] with
                | Ok l ->
                    match sAddNumber context pos id [ xVal; yVal ] with
                    | Ok r ->
                        match l, r with
                        | (SRational _, _), (SRational _, _) -> simplestRational pos cont (Ok l, Ok r)
                        | _ -> Ok x |> cont
                    | x -> x |> cont
                | x -> x |> cont
            | _ -> Ok x |> cont
        | x -> x |> invalidParameter pos "'%s' invalid rationalize parameter." |> cont

    let sExp context pos cont =
        function
        | [ SReal r, _ ] -> Ok(r |> exp |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> Ok(c |> System.Numerics.Complex.Exp |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> Ok(float n / float d |> exp |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid exp parameter." |> cont

    let sLog context pos cont =
        function
        | [ SReal r, _ ] -> Ok(r |> log |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> Ok(c |> System.Numerics.Complex.Log |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> Ok(float n / float d |> log |> SReal, pos) |> cont
        | [ x; y ] ->
            match toComplex x, toComplex y with
            | Ok c1, Ok c2 ->
                let res = System.Numerics.Complex.Log c1 / System.Numerics.Complex.Log c2
                Ok((if res.Imaginary = 0.0 then SReal res.Real else SComplex res), pos) |> cont
            | Error e, _ -> Error e |> cont
            | _, Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid log parameter." |> cont

    let sSin context pos cont =
        function
        | [ SReal r, _ ] -> Ok(r |> sin |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> Ok(c |> System.Numerics.Complex.Sin |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> Ok(float n / float d |> sin |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid sin parameter." |> cont

    let sCos context pos cont =
        function
        | [ SReal r, _ ] -> Ok(r |> cos |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> Ok(c |> System.Numerics.Complex.Cos |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> Ok(float n / float d |> cos |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid cos parameter." |> cont

    let sTan context pos cont =
        function
        | [ SReal r, _ ] -> Ok(r |> tan |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> Ok(c |> System.Numerics.Complex.Tan |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> Ok(float n / float d |> tan |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid tan parameter." |> cont

    let sAsin context pos cont =
        function
        | [ SReal r, _ ] when r >= -1.0 && r <= 1.0 -> Ok(r |> asin |> SReal, pos) |> cont
        | [ SReal r, _ ] ->
            Ok(System.Numerics.Complex(r, 0.0) |> System.Numerics.Complex.Asin |> SComplex, pos)
            |> cont
        | [ SComplex c, _ ] -> Ok(c |> System.Numerics.Complex.Asin |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] ->
            let r = float n / float d

            if r >= -1.0 && r <= 1.0 then
                Ok(r |> asin |> SReal, pos) |> cont
            else
                Ok(System.Numerics.Complex(r, 0.0) |> System.Numerics.Complex.Asin |> SComplex, pos)
                |> cont
        | x -> x |> invalidParameter pos "'%s' invalid asin parameter." |> cont

    let sAcos context pos cont =
        function
        | [ SReal r, _ ] when r >= -1.0 && r <= 1.0 -> Ok(r |> acos |> SReal, pos) |> cont
        | [ SReal r, _ ] ->
            Ok(System.Numerics.Complex(r, 0.0) |> System.Numerics.Complex.Acos |> SComplex, pos)
            |> cont
        | [ SComplex c, _ ] -> Ok(c |> System.Numerics.Complex.Acos |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] ->
            let r = float n / float d

            if r >= -1.0 && r <= 1.0 then
                Ok(r |> acos |> SReal, pos) |> cont
            else
                Ok(System.Numerics.Complex(r, 0.0) |> System.Numerics.Complex.Acos |> SComplex, pos)
                |> cont
        | x -> x |> invalidParameter pos "'%s' invalid acos parameter." |> cont

    let sAtan context pos cont =
        function
        | [ SReal r, _ ] -> Ok(r |> atan |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> Ok(c |> System.Numerics.Complex.Atan |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> Ok(float n / float d |> atan |> SReal, pos) |> cont
        | [ y; x ] ->
            let toFloat =
                function
                | SReal r, _ -> Ok r
                | SRational(n, d), _ -> float n / float d |> Ok
                | _, pos -> EvalError("atan expected real", pos) |> Error

            match toFloat y, toFloat x with
            | Ok yVal, Ok xVal -> Ok(System.Math.Atan2(yVal, xVal) |> SReal, pos) |> cont
            | Error e, _ -> Error e |> cont
            | _, Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid atan parameter." |> cont

    let sSquare context pos cont =
        function
        | [ x ] -> sMultiplyNumber context pos cont [ x; x ]
        | x -> x |> invalidParameter pos "'%s' invalid square parameter." |> cont

    let sSqrt context pos cont =
        function
        | [ SRational(n, d), _ ] when n >= 0I -> Ok(float n / float d |> sqrt |> SReal, pos) |> cont
        | [ SRational(n, d), _ ] ->
            Ok(
                (System.Numerics.Complex(float n / float d, 0.0)
                 |> System.Numerics.Complex.Sqrt
                 |> SComplex,
                 pos)
            )
            |> cont
        | [ SReal r, _ ] when r >= 0.0 -> Ok(r |> sqrt |> SReal, pos) |> cont
        | [ SReal r, _ ] ->
            Ok((System.Numerics.Complex(r, 0.0) |> System.Numerics.Complex.Sqrt |> SComplex, pos))
            |> cont
        | [ SComplex c, _ ] -> Ok(c |> System.Numerics.Complex.Sqrt |> SComplex, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid sqrt parameter." |> cont

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

    let sExpt context pos cont =
        function
        | [ x; y ] ->
            match x, y with
            | (SRational(n1, d1), _), (SRational(n2, d2), _) when d1 = 1I && d2 = 1I ->
                if n2 >= 0I then
                    Ok(bigint.Pow(n1, int n2) |> newInteger, pos) |> cont
                else if n1 = 0I then
                    EvalError("Division by zero in expt.", pos) |> Error |> cont
                else
                    newSRational 1I (bigint.Pow(n1, int -n2))
                    |> Result.map (fun r -> r, pos)
                    |> Result.mapError (fun msg -> EvalError(msg, pos))
                    |> cont
            | _ ->
                match toComplex x, toComplex y with
                | Ok c1, Ok c2 -> Ok(System.Numerics.Complex.Pow(c1, c2) |> SComplex, pos) |> cont
                | Error e, _ -> Error e |> cont
                | _, Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid expt parameter." |> cont

    let sMakeRectangular context pos cont =
        function
        | [ real; image ] ->
            match toComplex real, toComplex image with
            | Ok cr, Ok ci -> Ok(System.Numerics.Complex(cr.Real, ci.Real) |> SComplex, pos) |> cont
            | Error e, _ -> Error e |> cont
            | _, Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-rectangular parameter." |> cont

    let sMakePolar context pos cont =
        function
        | [ magnitude; angle ] ->
            match toComplex magnitude, toComplex angle with
            | Ok cm, Ok ca ->
                Ok(System.Numerics.Complex.FromPolarCoordinates(cm.Real, ca.Real) |> SComplex, pos)
                |> cont
            | Error e, _ -> Error e |> cont
            | _, Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-polar parameter." |> cont

    let sRealPart context pos cont =
        function
        | [ x ] -> toComplex x |> Result.map (fun c -> c.Real |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid real-part parameter." |> cont

    let sImagPart context pos cont =
        function
        | [ x ] -> toComplex x |> Result.map (fun c -> c.Imaginary |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid imag-part parameter." |> cont

    let sMagnitude context pos cont =
        function
        | [ x ] -> toComplex x |> Result.map (fun c -> c.Magnitude |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid magnitude parameter." |> cont

    let sAngle context pos cont =
        function
        | [ x ] -> toComplex x |> Result.map (fun c -> c.Phase |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid angle parameter." |> cont

    let sInexact context pos cont =
        function
        | [ SRational(n, d), _ ] -> Ok(float n / float d |> SReal, pos) |> cont
        | [ SReal _, _ ] as x -> Ok x.Head |> cont
        | [ SComplex _, _ ] as x -> Ok x.Head |> cont
        | x -> x |> invalidParameter pos "'%s' invalid inexact parameter." |> cont

    let sExact context pos cont =
        function
        | [ SRational _, _ ] as x -> Ok x.Head |> cont
        | [ SReal r, _ ] when finiteFloat r -> Ok(realToRational r, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid exact parameter." |> cont

    let sNumberToString context pos cont =
        function
        | [ n ] -> Ok(n |> Print.print |> newSString true, pos) |> cont
        | [ n; SRational(radix, d), _ ] when d = 1I ->
            match n with
            | SRational(n', d'), _ ->
                (if d' = 1I then
                     match int radix with
                     | 2 -> Ok(System.Convert.ToString(int64 n', 2))
                     | 8 -> Ok(System.Convert.ToString(int64 n', 8))
                     | 10 -> Ok(string n')
                     | 16 -> Ok(System.Convert.ToString(int64 n', 16))
                     | x -> EvalError(sprintf "'%d' unsupported radix in number->string." x, pos) |> Error
                 else
                     Ok(sprintf "%A/%A" n' d'))
                |> Result.map (fun s -> s |> newSString true, pos)
                |> cont
            | _ -> Ok(n |> Print.print |> newSString true, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid number->string parameter." |> cont

    let sStringToNumber context pos cont =
        function
        | [ SString s, _ ] ->
            s.runes
            |> runesToString
            |> Read.read false
            |> Result.map (function
                | SRational _, _
                | SReal _, _
                | SComplex _, _ as n -> n
                | _ -> SFalse, pos)
            |> function
                | Ok res -> Ok res |> cont
                | Error _ -> Ok(SFalse, pos) |> cont
        | [ SString data, _; SRational(radix, d), _ ] when d = 1I ->
            match int radix with
            | 2
            | 8
            | 10
            | 16 ->
                try
                    let s = data.runes |> runesToString

                    let v =
                        match int radix with
                        | 2 -> System.Convert.ToInt64(s, 2) |> bigint
                        | 8 -> System.Convert.ToInt64(s, 8) |> bigint
                        | 10 -> bigint.Parse s
                        | 16 -> System.Convert.ToInt64(s, 16) |> bigint
                        | _ -> failwith "unreachable."

                    Ok(newInteger v, pos) |> cont
                with _ ->
                    Ok(SFalse, pos) |> cont
            | _ -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string->number parameter." |> cont
