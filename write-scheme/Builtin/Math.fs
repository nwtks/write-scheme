namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Math =
    let isNumber envs pos cont =
        function
        | [ SRational _, _ ]
        | [ SReal _, _ ]
        | [ SComplex _, _ ] -> Ok(STrue, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let isComplex envs pos cont =
        function
        | [ SComplex _, _ ]
        | [ SReal _, _ ]
        | [ SRational _, _ ] -> Ok(STrue, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let isReal envs pos cont =
        function
        | [ SRational _, _ ]
        | [ SReal _, _ ] -> Ok(STrue, pos) |> cont
        | [ SComplex c, _ ] when c.Imaginary = 0.0 -> Ok(STrue, pos) |> cont
        | [ SComplex _, _ ] -> Ok(SFalse, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let finiteFloat d =
        not (System.Double.IsInfinity d || System.Double.IsNaN d)

    let noFractionFloat (d: float) = d = System.Math.Truncate d

    let isRational envs pos cont =
        function
        | [ SRational _, _ ] -> Ok(STrue, pos) |> cont
        | [ SReal r, _ ] when finiteFloat r -> Ok(STrue, pos) |> cont
        | [ SComplex c, _ ] when c.Imaginary = 0.0 && finiteFloat c.Real -> Ok(STrue, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let isInteger envs pos cont =
        function
        | [ SRational(_, d), _ ] when d = 1I -> Ok(STrue, pos) |> cont
        | [ SReal r, _ ] when finiteFloat r && noFractionFloat r -> Ok(STrue, pos) |> cont
        | [ SComplex c, _ ] when c.Imaginary = 0.0 && finiteFloat c.Real && noFractionFloat c.Real ->
            Ok(STrue, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let isExact envs pos cont =
        function
        | [ SRational _, _ ] -> Ok(STrue, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let isInexact envs pos cont =
        function
        | [ SReal _, _ ]
        | [ SComplex _, _ ] -> Ok(STrue, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let isExactInteger envs pos cont =
        function
        | [ SRational(_, d), _ ] when d = 1I -> Ok(STrue, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let isFinite envs pos cont =
        function
        | [ SRational _, _ ] -> Ok(STrue, pos) |> cont
        | [ SReal r, _ ] -> Ok(finiteFloat r |> toSBool, pos) |> cont
        | [ SComplex c, _ ] -> Ok((finiteFloat c.Real && finiteFloat c.Imaginary) |> toSBool, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let isInfinite envs pos cont =
        function
        | [ SReal r, _ ] -> Ok(System.Double.IsInfinity r |> toSBool, pos) |> cont
        | [ SComplex c, _ ] ->
            Ok(
                ((System.Double.IsInfinity c.Real || System.Double.IsInfinity c.Imaginary)
                 |> toSBool,
                 pos)
            )
            |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let isNaN envs pos cont =
        function
        | [ SReal r, _ ] -> Ok(System.Double.IsNaN r |> toSBool, pos) |> cont
        | [ SComplex c, _ ] ->
            Ok(((System.Double.IsNaN c.Real || System.Double.IsNaN c.Imaginary) |> toSBool, pos))
            |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let toFloat x y = float x / float y

    let toComplex =
        function
        | SRational(x1, x2), _ -> Ok(System.Numerics.Complex(toFloat x1 x2, 0.0))
        | SReal x, _ -> Ok(System.Numerics.Complex(x, 0.0))
        | SComplex c, _ -> Ok c
        | x -> x |> invalid (snd x) "'%s' is not a number."

    let comparePred pred1 pred2 pred3 =
        function
        | (SRational(a1, a2), _), (SRational(b1, b2), _) -> Ok(pred1 (a1 * b2) (b1 * a2))
        | (SRational(a1, a2), _), (SReal b, _) -> Ok(pred2 (toFloat a1 a2) b)
        | (SReal a, _), (SRational(b1, b2), _) -> Ok(pred2 a (toFloat b1 b2))
        | (SReal a, _), (SReal b, _) -> Ok(pred2 a b)
        | (SComplex _, _), _
        | _, (SComplex _, _) as pair ->
            match toComplex (fst pair), toComplex (snd pair) with
            | Ok ca, Ok cb -> pred3 ca cb
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
            Ok(op x.Real y.Real)
        else
            Error(EvalError("Ordering on complex numbers with non-zero imaginary parts is undefined.", None))

    let equalNumber envs =
        compareNumber (=) (=) (fun x y -> Ok(x = y))

    let lessNumber envs = compareNumber (<) (<) (complexReal (<))

    let greaterNumber envs = compareNumber (>) (>) (complexReal (>))

    let lessEqualNumber envs =
        compareNumber (<=) (<=) (complexReal (<=))

    let greaterEqualNumber envs =
        compareNumber (>=) (>=) (complexReal (>=))

    let isZero envs pos cont =
        function
        | [ x ] -> equalNumber envs pos cont [ x; SZero, pos ]
        | _ -> Ok(SFalse, pos) |> cont

    let isPositive envs pos cont =
        function
        | [ x ] -> greaterNumber envs pos cont [ x; SZero, pos ]
        | _ -> Ok(SFalse, pos) |> cont

    let isNegative envs pos cont =
        function
        | [ x ] -> lessNumber envs pos cont [ x; SZero, pos ]
        | _ -> Ok(SFalse, pos) |> cont

    let isOdd envs pos cont =
        function
        | [ SRational(n, d), _ ] when d = 1I -> Ok(abs n % 2I = 1I |> toSBool, pos) |> cont
        | [ SReal r, _ ] when finiteFloat r && noFractionFloat r -> Ok(abs r % 2.0 = 1.0 |> toSBool, pos) |> cont
        | [ SComplex c, _ ] when c.Imaginary = 0.0 && finiteFloat c.Real && noFractionFloat c.Real ->
            Ok(abs c.Real % 2.0 = 1.0 |> toSBool, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid odd? parameter." |> cont

    let isEven envs pos cont =
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

    let sMax envs pos cont args =
        if List.isEmpty args then
            args |> invalidParameter pos "'%s' invalid max parameter." |> cont
        else
            match args with
            | h :: t ->
                t
                |> findMaxValue h
                |> Result.map (fun maxVal ->
                    if isAnyInexact args then
                        match maxVal with
                        | SRational(n, d), _ -> SReal(float n / float d), pos
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

    let sMin envs pos cont args =
        if List.isEmpty args then
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
        let wrap res =
            res
            |> Result.map (fun n -> n, pos)
            |> Result.mapError (fun msg -> EvalError(msg, pos))

        function
        | [] -> acc |> cont
        | (y, pos') :: xs ->
            match acc, y with
            | Ok(SRational(a1, a2), _), SRational(b1, b2) ->
                xs |> loopCalc op1 op2 op3 pos cont (op1 a1 a2 b1 b2 |> wrap)
            | Ok(SRational(a1, a2), _), SReal b -> xs |> loopCalc op1 op2 op3 pos cont (op2 (toFloat a1 a2) b |> wrap)
            | Ok(SReal a, _), SRational(b1, b2) -> xs |> loopCalc op1 op2 op3 pos cont (op2 a (toFloat b1 b2) |> wrap)
            | Ok(SReal a, _), SReal b -> xs |> loopCalc op1 op2 op3 pos cont (op2 a b |> wrap)
            | Ok(SComplex _, _), _
            | Ok _, SComplex _ ->
                match acc |> Result.bind toComplex, toComplex (y, pos') with
                | Ok ca, Ok cb -> xs |> loopCalc op1 op2 op3 pos cont (wrap (op3 ca cb))
                | Error e as x, _ -> Error e |> cont
                | _, Error e -> Error e |> cont
            | Ok a, b ->
                Error(EvalError(sprintf "'%s', '%s' not number." (a |> Print.print) ((b, pos') |> Print.print), pos))
                |> cont
            | x, _ -> x |> cont

    let calc op1 op2 op3 ident1 ident2 ident3 pos cont =
        let wrap res =
            res
            |> Result.map (fun n -> n, pos)
            |> Result.mapError (fun msg -> EvalError(msg, pos))

        function
        | [] -> Ok(newInteger ident1, pos) |> cont
        | [ SRational(x1, x2), _ ] -> op1 ident1 1I x1 x2 |> wrap |> cont
        | [ SReal x, _ ] -> op2 ident2 x |> wrap |> cont
        | [ SComplex c, _ ] -> op3 ident3 c |> wrap |> cont
        | x :: xs -> xs |> loopCalc op1 op2 op3 pos cont (Ok x)

    let addNumber envs =
        calc
            (fun a1 a2 b1 b2 -> newSRational (a1 * b2 + b1 * a2) (a2 * b2))
            (fun n1 n2 -> n1 + n2 |> SReal |> Ok)
            (fun c1 c2 -> c1 + c2 |> SComplex |> Ok)
            0I
            0.0
            System.Numerics.Complex.Zero

    let multiplyNumber envs =
        calc
            (fun a1 a2 b1 b2 -> newSRational (a1 * b1) (a2 * b2))
            (fun n1 n2 -> n1 * n2 |> SReal |> Ok)
            (fun c1 c2 -> c1 * c2 |> SComplex |> Ok)
            1I
            1.0
            System.Numerics.Complex.One

    let subtractNumber envs =
        calc
            (fun a1 a2 b1 b2 -> newSRational (a1 * b2 - b1 * a2) (a2 * b2))
            (fun n1 n2 -> n1 - n2 |> SReal |> Ok)
            (fun c1 c2 -> c1 - c2 |> SComplex |> Ok)
            0I
            0.0
            System.Numerics.Complex.Zero

    let divideNumber envs =
        calc
            (fun a1 a2 b1 b2 -> newSRational (a1 * b2) (a2 * b1))
            (fun n1 n2 ->
                if n2 = 0.0 then
                    Error "Division by zero."
                else
                    n1 / n2 |> SReal |> Ok)
            (fun c1 c2 ->
                if c2.Magnitude = 0.0 then
                    Error "Division by zero."
                else
                    c1 / c2 |> SComplex |> Ok)
            1I
            1.0
            System.Numerics.Complex.One

    let sAbs envs pos cont =
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
        let q, r = truncateDiv n d
        if r <> 0I && n.Sign <> d.Sign then q - 1I, r + d else q, r

    let sFloorDiv envs pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            let q, r = floorDiv n1 n2
            Ok(SValues [ newInteger q, pos; newInteger r, pos ], pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid floor/ parameter." |> cont

    let sFloorQuotient envs pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            let q, _ = floorDiv n1 n2
            Ok(newInteger q, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid floor-quotient parameter." |> cont

    let sFloorRemainder envs pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            let _, r = floorDiv n1 n2
            Ok(newInteger r, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid floor-remainder parameter." |> cont

    let sTruncateDiv envs pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            let q, r = truncateDiv n1 n2
            Ok(SValues [ newInteger q, pos; newInteger r, pos ], pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid truncate/ parameter." |> cont

    let sTruncateQuotient envs pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            let q, _ = truncateDiv n1 n2
            Ok(newInteger q, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid truncate-quotient parameter." |> cont

    let sTruncateRemainder envs pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            let _, r = truncateDiv n1 n2
            Ok(newInteger r, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid truncate-remainder parameter." |> cont

    let sQuotient envs pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            Ok(newInteger (n1 / n2), pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid quotient parameter." |> cont

    let sRemainder envs pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            Ok(newInteger (n1 % n2), pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid remainder parameter." |> cont

    let sModulo envs pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            let r = n1 % n2

            if r <> 0I && n1.Sign <> n2.Sign then
                Ok(newInteger (r + n2), pos) |> cont
            else
                Ok(newInteger r, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid modulo parameter." |> cont

    let gcd a b = bigint.GreatestCommonDivisor(a, b)

    let sGcd envs pos cont =
        mapResult (function
            | SRational(n, d), _ when d = 1I -> Ok n
            | x -> x |> invalid (snd x) "'%s' is not an integer in gcd.")
        >> Result.map (List.fold gcd 0I >> fun v -> newInteger v, pos)
        >> cont

    let lcm a b =
        if a = 0I || b = 0I then 0I else abs (a * b) / gcd a b

    let sLcm envs pos cont =
        mapResult (function
            | SRational(n, d), _ when d = 1I -> Ok n
            | x -> x |> invalid (snd x) "'%s' is not an integer in lcm.")
        >> Result.map (List.fold lcm 1I >> fun v -> newInteger v, pos)
        >> cont

    let sNumerator envs pos cont =
        function
        | [ SRational(n, _), _ ] -> Ok(newInteger n, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid numerator parameter." |> cont

    let sDenominator envs pos cont =
        function
        | [ SRational(_, d), _ ] -> Ok(newInteger d, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid denominator parameter." |> cont

    let sFloor envs pos cont =
        function
        | [ SRational(n, d), _ ] ->
            let q, r = truncateDiv n d

            if r <> 0I && n.Sign <> d.Sign then q - 1I else q
            |> fun v -> Ok(newInteger v, pos) |> cont
        | [ SReal r, _ ] -> Ok(r |> floor |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid floor parameter." |> cont

    let sCeiling envs pos cont =
        function
        | [ SRational(n, d), _ ] ->
            let q, r = truncateDiv n d

            if r <> 0I && n.Sign = d.Sign then q + 1I else q
            |> fun v -> Ok(newInteger v, pos) |> cont
        | [ SReal r, _ ] -> Ok(r |> ceil |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid ceiling parameter." |> cont

    let sTruncate envs pos cont =
        function
        | [ SRational(n, d), _ ] -> Ok(newInteger (n / d), pos) |> cont
        | [ SReal r, _ ] -> Ok(r |> truncate |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid truncate parameter." |> cont

    let sRound envs pos cont =
        function
        | [ SRational(n, d), _ ] -> Ok(newInteger (float n / float d |> round |> bigint), pos) |> cont
        | [ SReal r, _ ] -> Ok(r |> round |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid round parameter." |> cont

    [<TailCall>]
    let rec simplestRational l r pos cont =
        match l, r with
        | Ok(SRational(nl, dl), _), Ok(SRational(nr, dr), _) ->
            let floorL = if nl >= 0I then nl / dl else (nl - dl + 1I) / dl
            let ceilL = if nl % dl = 0I then floorL else floorL + 1I
            let floorR = if nr >= 0I then nr / dr else (nr - dr + 1I) / dr

            if ceilL <= floorR then
                if ceilL > 0I then Ok(newInteger ceilL, pos) |> cont
                elif floorR < 0I then Ok(newInteger floorR, pos) |> cont
                else Ok(SZero, pos) |> cont
            else
                simplestRational
                    (newSRational dr (nr - floorL * dr)
                     |> Result.map (fun n -> n, pos)
                     |> Result.mapError (fun msg -> EvalError(msg, pos)))
                    (newSRational dl (nl - floorL * dl)
                     |> Result.map (fun n -> n, pos)
                     |> Result.mapError (fun msg -> EvalError(msg, pos)))
                    pos
                    (function
                     | Ok(SRational(pn, pd), _) ->
                         newSRational (floorL * pn + pd) pn
                         |> Result.map (fun n -> n, pos)
                         |> Result.mapError (fun msg -> EvalError(msg, pos))
                         |> cont
                     | x -> x |> cont)
        | _ -> Error(EvalError("Operands not rational", pos)) |> cont

    let sRationalize envs pos cont =
        let toExactValue =
            function
            | SRational _, _ as x -> Ok x
            | SReal r, _ when finiteFloat r -> Ok(realToRational r, pos)
            | x -> Ok x

        function
        | [ x; e ] ->
            match toExactValue x, toExactValue e with
            | Ok(SRational _, _ as xVal), Ok(SRational _, _ as eVal) ->
                match subtractNumber envs pos id [ xVal; eVal ] with
                | Ok l ->
                    match addNumber envs pos id [ xVal; eVal ] with
                    | Ok r ->
                        match l, r with
                        | (SRational _, _), (SRational _, _) -> simplestRational (Ok l) (Ok r) pos cont
                        | _ -> Ok x |> cont
                    | Error e' -> Error e' |> cont
                | Error e' -> Error e' |> cont
            | _ -> Ok x |> cont
        | x -> x |> invalidParameter pos "'%s' invalid rationalize parameter." |> cont

    let sExp envs pos cont =
        function
        | [ SReal r, _ ] -> Ok(r |> exp |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> Ok(c |> System.Numerics.Complex.Exp |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> Ok(float n / float d |> exp |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid exp parameter." |> cont

    let sLog envs pos cont =
        function
        | [ SReal r, _ ] -> Ok(r |> log |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> Ok(c |> System.Numerics.Complex.Log |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> Ok(float n / float d |> log |> SReal, pos) |> cont
        | [ x; b ] ->
            match toComplex x, toComplex b with
            | Ok cx, Ok cb ->
                let res = System.Numerics.Complex.Log cx / System.Numerics.Complex.Log cb
                Ok((if res.Imaginary = 0.0 then SReal res.Real else SComplex res), pos) |> cont
            | Error e, _ -> Error e |> cont
            | _, Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid log parameter." |> cont

    let sSin envs pos cont =
        function
        | [ SReal r, _ ] -> Ok(r |> sin |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> Ok(c |> System.Numerics.Complex.Sin |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> Ok(float n / float d |> sin |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid sin parameter." |> cont

    let sCos envs pos cont =
        function
        | [ SReal r, _ ] -> Ok(r |> cos |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> Ok(c |> System.Numerics.Complex.Cos |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> Ok(float n / float d |> cos |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid cos parameter." |> cont

    let sTan envs pos cont =
        function
        | [ SReal r, _ ] -> Ok(r |> tan |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> Ok(c |> System.Numerics.Complex.Tan |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> Ok(float n / float d |> tan |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid tan parameter." |> cont

    let sAsin envs pos cont =
        function
        | [ SReal r, _ ] -> Ok(r |> asin |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> Ok(c |> System.Numerics.Complex.Asin |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> Ok(float n / float d |> asin |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid asin parameter." |> cont

    let sAcos envs pos cont =
        function
        | [ SReal r, _ ] -> Ok(r |> acos |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> Ok(c |> System.Numerics.Complex.Acos |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> Ok(float n / float d |> acos |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid acos parameter." |> cont

    let sAtan envs pos cont =
        function
        | [ SReal r, _ ] -> Ok(r |> atan |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> Ok(c |> System.Numerics.Complex.Atan |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> Ok(float n / float d |> atan |> SReal, pos) |> cont
        | [ yExpr; xExpr ] ->
            let getY =
                match yExpr with
                | SReal r, _ -> Ok r
                | SRational(n, d), _ -> Ok(float n / float d)
                | _, pos -> Error(EvalError("atan expected real", pos))

            let getX =
                match xExpr with
                | SReal r, _ -> Ok r
                | SRational(n, d), _ -> Ok(float n / float d)
                | _, pos -> Error(EvalError("atan expected real", pos))

            match getY, getX with
            | Ok yVal, Ok xVal -> Ok(System.Math.Atan2(yVal, xVal) |> SReal, pos) |> cont
            | Error e, _ -> Error e |> cont
            | _, Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid atan parameter." |> cont

    let sSquare envs pos cont =
        function
        | [ x ] -> multiplyNumber envs pos cont [ x; x ]
        | x -> x |> invalidParameter pos "'%s' invalid square parameter." |> cont

    let sSqrt envs pos cont =
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

    let sExactIntegerSqrt envs pos cont =
        function
        | [ SRational(k, d), _ ] when d = 1I && k >= 0I ->
            let s = if k < 2I then k else bigintSqrt 1I (k + 1I) k
            let r = k - s * s
            Ok(SValues [ newInteger s, pos; newInteger r, pos ], pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid exact-integer-sqrt parameter." |> cont

    let sExpt envs pos cont =
        function
        | [ x; y ] ->
            match x, y with
            | (SRational(n1, d1), _), (SRational(n2, d2), _) when d1 = 1I && d2 = 1I && n2 >= 0I ->
                Ok(newInteger (bigint.Pow(n1, int n2)), pos) |> cont
            | _ ->
                match toComplex x, toComplex y with
                | Ok cx, Ok cy -> Ok(System.Numerics.Complex.Pow(cx, cy) |> SComplex, pos) |> cont
                | Error e, _ -> Error e |> cont
                | _, Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid expt parameter." |> cont

    let sMakeRectangular envs pos cont =
        function
        | [ r; i ] ->
            match toComplex r, toComplex i with
            | Ok cr, Ok ci -> Ok(System.Numerics.Complex(cr.Real, ci.Real) |> SComplex, pos) |> cont
            | Error e, _ -> Error e |> cont
            | _, Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-rectangular parameter." |> cont

    let sMakePolar envs pos cont =
        function
        | [ r; theta ] ->
            match toComplex r, toComplex theta with
            | Ok cr, Ok ct ->
                Ok(System.Numerics.Complex.FromPolarCoordinates(cr.Real, ct.Real) |> SComplex, pos)
                |> cont
            | Error e, _ -> Error e |> cont
            | _, Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-polar parameter." |> cont

    let sRealPart envs pos cont =
        function
        | [ x ] -> toComplex x |> Result.map (fun c -> c.Real |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid real-part parameter." |> cont

    let sImagPart envs pos cont =
        function
        | [ x ] -> toComplex x |> Result.map (fun c -> c.Imaginary |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid imag-part parameter." |> cont

    let sMagnitude envs pos cont =
        function
        | [ x ] -> toComplex x |> Result.map (fun c -> c.Magnitude |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid magnitude parameter." |> cont

    let sAngle envs pos cont =
        function
        | [ x ] -> toComplex x |> Result.map (fun c -> c.Phase |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid angle parameter." |> cont

    let sInexact envs pos cont =
        function
        | [ SRational(n, d), _ ] -> Ok(SReal(float n / float d), pos) |> cont
        | [ SReal _, _ ] as x -> Ok x.Head |> cont
        | [ SComplex _, _ ] as x -> Ok x.Head |> cont
        | x -> x |> invalidParameter pos "'%s' invalid inexact parameter." |> cont

    let sExact envs pos cont =
        function
        | [ SRational _, _ ] as x -> Ok x.Head |> cont
        | [ SReal r, _ ] when finiteFloat r -> Ok(realToRational r, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid exact parameter." |> cont

    let sNumberToString envs pos cont =
        function
        | [ n ] -> Ok(n |> Print.print |> newSString true, pos) |> cont
        | [ n; SRational(radix, d), _ ] when d = 1I ->
            match n with
            | SRational(k, d'), _ ->
                (if d' = 1I then
                     match int radix with
                     | 2 -> Ok(System.Convert.ToString(int64 k, 2))
                     | 8 -> Ok(System.Convert.ToString(int64 k, 8))
                     | 10 -> Ok(string k)
                     | 16 -> Ok(System.Convert.ToString(int64 k, 16))
                     | x -> Error(EvalError(sprintf "'%d' unsupported radix in number->string." x, pos))
                 else
                     Ok(sprintf "%A/%A" k d'))
                |> Result.map (fun s -> newSString true s, pos)
                |> cont
            | _ -> Ok(n |> Print.print |> newSString true, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid number->string parameter." |> cont

    let sStringToNumber envs pos cont =
        function
        | [ SString s, _ ] ->
            s.runes
            |> runesToString
            |> Read.read
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
