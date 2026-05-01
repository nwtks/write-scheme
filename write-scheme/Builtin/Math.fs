namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Math =
    let isNumber envs pos cont =
        function
        | [ SRational _, _ ]
        | [ SReal _, _ ]
        | [ SComplex _, _ ] -> (STrue, pos) |> cont
        | _ -> (SFalse, pos) |> cont

    let isComplex envs pos cont =
        function
        | [ SComplex _, _ ]
        | [ SReal _, _ ]
        | [ SRational _, _ ] -> (STrue, pos) |> cont
        | _ -> (SFalse, pos) |> cont

    let isReal envs pos cont =
        function
        | [ SRational _, _ ]
        | [ SReal _, _ ] -> (STrue, pos) |> cont
        | [ SComplex c, _ ] when c.Imaginary = 0.0 -> (STrue, pos) |> cont
        | [ SComplex _, _ ] -> (SFalse, pos) |> cont
        | _ -> (SFalse, pos) |> cont

    let finiteFloat d =
        not (System.Double.IsInfinity d || System.Double.IsNaN d)

    let noFractionFloat (d: float) = d = System.Math.Truncate d

    let isRational envs pos cont =
        function
        | [ SRational _, _ ] -> (STrue, pos) |> cont
        | [ SReal r, _ ] when finiteFloat r -> (STrue, pos) |> cont
        | [ SComplex c, _ ] when c.Imaginary = 0.0 && finiteFloat c.Real -> (STrue, pos) |> cont
        | _ -> (SFalse, pos) |> cont

    let isInteger envs pos cont =
        function
        | [ SRational(_, d), _ ] when d = 1I -> (STrue, pos) |> cont
        | [ SReal r, _ ] when finiteFloat r && noFractionFloat r -> (STrue, pos) |> cont
        | [ SComplex c, _ ] when c.Imaginary = 0.0 && finiteFloat c.Real && noFractionFloat c.Real ->
            (STrue, pos) |> cont
        | _ -> (SFalse, pos) |> cont

    let isExact envs pos cont =
        function
        | [ SRational _, _ ] -> (STrue, pos) |> cont
        | _ -> (SFalse, pos) |> cont

    let isInexact envs pos cont =
        function
        | [ SReal _, _ ]
        | [ SComplex _, _ ] -> (STrue, pos) |> cont
        | _ -> (SFalse, pos) |> cont

    let isExactInteger envs pos cont =
        function
        | [ SRational(_, d), _ ] when d = 1I -> (STrue, pos) |> cont
        | _ -> (SFalse, pos) |> cont

    let isFinite envs pos cont =
        function
        | [ SRational _, _ ] -> (STrue, pos) |> cont
        | [ SReal r, _ ] -> (finiteFloat r |> toSBool, pos) |> cont
        | [ SComplex c, _ ] -> ((finiteFloat c.Real && finiteFloat c.Imaginary) |> toSBool, pos) |> cont
        | _ -> (SFalse, pos) |> cont

    let isInfinite envs pos cont =
        function
        | [ SReal r, _ ] -> (System.Double.IsInfinity r |> toSBool, pos) |> cont
        | [ SComplex c, _ ] ->
            ((System.Double.IsInfinity c.Real || System.Double.IsInfinity c.Imaginary)
             |> toSBool,
             pos)
            |> cont
        | _ -> (SFalse, pos) |> cont

    let isNaN envs pos cont =
        function
        | [ SReal r, _ ] -> (System.Double.IsNaN r |> toSBool, pos) |> cont
        | [ SComplex c, _ ] ->
            ((System.Double.IsNaN c.Real || System.Double.IsNaN c.Imaginary) |> toSBool, pos)
            |> cont
        | _ -> (SFalse, pos) |> cont

    let toFloat x y = float x / float y

    let toComplex =
        function
        | SRational(x1, x2), _ -> System.Numerics.Complex(toFloat x1 x2, 0.0)
        | SReal x, _ -> System.Numerics.Complex(x, 0.0)
        | SComplex c, _ -> c
        | x -> x |> invalid (snd x) "'%s' is not a number."

    let comparePred pred1 pred2 pred3 =
        function
        | (SRational(a1, a2), _), (SRational(b1, b2), _) -> pred1 (a1 * b2) (b1 * a2)
        | (SRational(a1, a2), _), (SReal b, _) -> pred2 (toFloat a1 a2) b
        | (SReal a, _), (SRational(b1, b2), _) -> pred2 a (toFloat b1 b2)
        | (SReal a, _), (SReal b, _) -> pred2 a b
        | (SComplex _, _), _
        | _, (SComplex _, _) as pair -> pred3 (fst pair |> toComplex) (snd pair |> toComplex)
        | _ -> false

    [<TailCall>]
    let rec compare pos pred1 pred2 pred3 n =
        function
        | [] -> STrue, pos
        | x :: xs ->
            if comparePred pred1 pred2 pred3 (n, x) then
                compare pos pred1 pred2 pred3 x xs
            else
                SFalse, pos

    let compareNumber pred1 pred2 pred3 pos cont =
        function
        | [] -> (STrue, pos) |> cont
        | x :: xs -> xs |> compare pos pred1 pred2 pred3 x |> cont

    let complexReal op (x: System.Numerics.Complex) (y: System.Numerics.Complex) =
        if x.Imaginary = 0.0 && y.Imaginary = 0.0 then
            op x.Real y.Real
        else
            failwith "Ordering on complex numbers with non-zero imaginary parts is undefined."

    let equalNumber envs = compareNumber (=) (=) (=)

    let lessNumber envs = compareNumber (<) (<) (complexReal (<))

    let greaterNumber envs = compareNumber (>) (>) (complexReal (>))

    let lessEqualNumber envs =
        compareNumber (<=) (<=) (complexReal (<=))

    let greaterEqualNumber envs =
        compareNumber (>=) (>=) (complexReal (>=))

    let isZero envs pos cont =
        function
        | [ x ] -> equalNumber envs pos cont [ x; SZero, pos ]
        | _ -> (SFalse, pos) |> cont

    let isPositive envs pos cont =
        function
        | [ x ] -> greaterNumber envs pos cont [ x; SZero, pos ]
        | _ -> (SFalse, pos) |> cont

    let isNegative envs pos cont =
        function
        | [ x ] -> lessNumber envs pos cont [ x; SZero, pos ]
        | _ -> (SFalse, pos) |> cont

    let isOdd envs pos cont =
        function
        | [ SRational(n, d), _ ] when d = 1I -> (abs n % 2I = 1I |> toSBool, pos) |> cont
        | [ SReal r, _ ] when finiteFloat r && noFractionFloat r -> (abs r % 2.0 = 1.0 |> toSBool, pos) |> cont
        | [ SComplex c, _ ] when c.Imaginary = 0.0 && finiteFloat c.Real && noFractionFloat c.Real ->
            (abs c.Real % 2.0 = 1.0 |> toSBool, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid odd? parameter."

    let isEven envs pos cont =
        function
        | [ SRational(n, d), _ ] when d = 1I -> (n % 2I = 0I |> toSBool, pos) |> cont
        | [ SReal r, _ ] when finiteFloat r && noFractionFloat r -> (r % 2.0 = 0.0 |> toSBool, pos) |> cont
        | [ SComplex c, _ ] when c.Imaginary = 0.0 && finiteFloat c.Real && noFractionFloat c.Real ->
            (c.Real % 2.0 = 0.0 |> toSBool, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid even? parameter."

    let isAnyInexact =
        List.exists (function
            | SRational _, _ -> false
            | _ -> true)

    let sMax envs pos cont args =
        if List.isEmpty args then
            args |> invalidParameter pos "'%s' invalid max parameter."
        else
            let maxVal =
                args
                |> List.reduce (fun acc x ->
                    if comparePred (>) (>) (complexReal (>)) (acc, x) then
                        acc
                    else
                        x)

            if isAnyInexact args then
                match maxVal with
                | SRational(n, d), _ -> SReal(float n / float d), pos
                | x -> x
            else
                maxVal
            |> cont

    let sMin envs pos cont args =
        if List.isEmpty args then
            args |> invalidParameter pos "'%s' invalid min parameter."
        else
            let minVal =
                args
                |> List.reduce (fun acc x ->
                    if comparePred (<) (<) (complexReal (<)) (acc, x) then
                        acc
                    else
                        x)

            if isAnyInexact args then
                match minVal with
                | SRational(n, d), _ -> SReal(float n / float d), pos
                | x -> x
            else
                minVal
            |> cont

    let calc op1 op2 op3 ident1 ident2 ident3 pos cont =
        let op x y =
            match x, y with
            | (SRational(a1, a2), _), (SRational(b1, b2), _) -> op1 a1 a2 b1 b2, pos
            | (SRational(a1, a2), _), (SReal b, _) -> op2 (toFloat a1 a2) b, pos
            | (SReal a, _), (SRational(b1, b2), _) -> op2 a (toFloat b1 b2), pos
            | (SReal a, _), (SReal b, _) -> op2 a b, pos
            | (SComplex _, _), (_, _) -> op3 (toComplex x) (toComplex y), pos
            | (_, _), (SComplex _, _) -> op3 (toComplex x) (toComplex y), pos
            | a, b ->
                failwithf "'%s', '%s' not number. %s" (a |> Print.print) (b |> Print.print) (a |> snd |> formatPosition)

        function
        | [] -> (SRational(ident1, 1I), pos) |> cont
        | [ SRational(x1, x2), _ ] -> (op1 ident1 1I x1 x2, pos) |> cont
        | [ SReal x, _ ] -> (op2 ident2 x, pos) |> cont
        | [ SComplex c, _ ] -> (op3 ident3 c, pos) |> cont
        | x :: xs -> List.fold op x xs |> cont

    let addNumber envs =
        calc
            (fun a1 a2 b1 b2 -> newSRational (a1 * b2 + b1 * a2) (a2 * b2))
            (fun n1 n2 -> n1 + n2 |> SReal)
            (fun c1 c2 -> c1 + c2 |> SComplex)
            0I
            0.0
            System.Numerics.Complex.Zero

    let multiplyNumber envs =
        calc
            (fun a1 a2 b1 b2 -> newSRational (a1 * b1) (a2 * b2))
            (fun n1 n2 -> n1 * n2 |> SReal)
            (fun c1 c2 -> c1 * c2 |> SComplex)
            1I
            1.0
            System.Numerics.Complex.One

    let subtractNumber envs =
        calc
            (fun a1 a2 b1 b2 -> newSRational (a1 * b2 - b1 * a2) (a2 * b2))
            (fun n1 n2 -> n1 - n2 |> SReal)
            (fun c1 c2 -> c1 - c2 |> SComplex)
            0I
            0.0
            System.Numerics.Complex.Zero

    let divideNumber envs =
        calc
            (fun a1 a2 b1 b2 -> newSRational (a1 * b2) (a2 * b1))
            (fun n1 n2 -> n1 / n2 |> SReal)
            (fun c1 c2 -> c1 / c2 |> SComplex)
            1I
            1.0
            System.Numerics.Complex.One

    let sAbs envs pos cont =
        function
        | [ SRational(n, d), _ ] -> (newSRational (abs n) d, pos) |> cont
        | [ SReal r, _ ] -> (abs r |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> (c.Magnitude |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid abs parameter."

    let truncateDiv n d = n / d, n % d

    let floorDiv (n: bigint) (d: bigint) =
        let q, r = truncateDiv n d
        if r <> 0I && n.Sign <> d.Sign then q - 1I, r + d else q, r

    let sFloorDiv envs pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            let q, r = floorDiv n1 n2
            (SValues [ newSRational q 1I, pos; newSRational r 1I, pos ], pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid floor/ parameter."

    let sFloorQuotient envs pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            let q, _ = floorDiv n1 n2
            (newSRational q 1I, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid floor-quotient parameter."

    let sFloorRemainder envs pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            let _, r = floorDiv n1 n2
            (newSRational r 1I, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid floor-remainder parameter."

    let sTruncateDiv envs pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            let q, r = truncateDiv n1 n2
            (SValues [ newSRational q 1I, pos; newSRational r 1I, pos ], pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid truncate/ parameter."

    let sTruncateQuotient envs pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            let q, _ = truncateDiv n1 n2
            (newSRational q 1I, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid truncate-quotient parameter."

    let sTruncateRemainder envs pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            let _, r = truncateDiv n1 n2
            (newSRational r 1I, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid truncate-remainder parameter."

    let sQuotient envs pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            (newSRational (n1 / n2) 1I, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid quotient parameter."

    let sRemainder envs pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            (newSRational (n1 % n2) 1I, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid remainder parameter."

    let sModulo envs pos cont =
        function
        | [ SRational(n1, d1), _; SRational(n2, d2), _ ] when d1 = 1I && d2 = 1I ->
            let r = n1 % n2

            if r <> 0I && n1.Sign <> n2.Sign then
                newSRational (r + n2) 1I, pos
            else
                newSRational r 1I, pos
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid modulo parameter."

    let gcd a b = bigint.GreatestCommonDivisor(a, b)

    let sGcd envs pos cont =
        List.map (function
            | SRational(n, d), _ when d = 1I -> n
            | x -> x |> invalid (snd x) "'%s' is not an integer in gcd.")
        >> List.fold gcd 0I
        >> fun v -> newSRational v 1I, pos
        >> cont

    let lcm a b =
        if a = 0I || b = 0I then 0I else abs (a * b) / gcd a b

    let sLcm envs pos cont =
        List.map (function
            | SRational(n, d), _ when d = 1I -> n
            | x -> x |> invalid (snd x) "'%s' is not an integer in lcm.")
        >> List.fold lcm 1I
        >> fun v -> newSRational v 1I, pos
        >> cont

    let sNumerator envs pos cont =
        function
        | [ SRational(n, _), _ ] -> (newSRational n 1I, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid numerator parameter."

    let sDenominator envs pos cont =
        function
        | [ SRational(_, d), _ ] -> (newSRational d 1I, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid denominator parameter."

    let sFloor envs pos cont =
        function
        | [ SRational(n, d), _ ] ->
            let q, r = truncateDiv n d

            if r <> 0I && n.Sign <> d.Sign then q - 1I else q
            |> fun v -> newSRational v 1I, pos
            |> cont
        | [ SReal r, _ ] -> (r |> floor |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid floor parameter."

    let sCeiling envs pos cont =
        function
        | [ SRational(n, d), _ ] ->
            let q, r = truncateDiv n d

            if r <> 0I && n.Sign = d.Sign then q + 1I else q
            |> fun v -> newSRational v 1I, pos
            |> cont
        | [ SReal r, _ ] -> (r |> ceil |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid ceiling parameter."

    let sTruncate envs pos cont =
        function
        | [ SRational(n, d), _ ] -> (newSRational (n / d) 1I, pos) |> cont
        | [ SReal r, _ ] -> (r |> truncate |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid truncate parameter."

    let sRound envs pos cont =
        function
        | [ SRational(n, d), _ ] -> (newSRational (float n / float d |> round |> bigint) 1I, pos) |> cont
        | [ SReal r, _ ] -> (r |> round |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid round parameter."

    [<TailCall>]
    let rec simplestRational l r pos cont =
        let nl, dl =
            match l with
            | SRational(n, d), _ -> n, d
            | x -> x |> invalid (snd x) " '%s' is not a rational."

        let nr, dr =
            match r with
            | SRational(n, d), _ -> n, d
            | x -> x |> invalid (snd x) " '%s' is not a rational."

        let floorL = if nl >= 0I then nl / dl else (nl - dl + 1I) / dl
        let ceilL = if nl % dl = 0I then floorL else floorL + 1I
        let floorR = if nr >= 0I then nr / dr else (nr - dr + 1I) / dr

        if ceilL <= floorR then
            if ceilL > 0I then (newSRational ceilL 1I, pos) |> cont
            elif floorR < 0I then (newSRational floorR 1I, pos) |> cont
            else (newSRational 0I 1I, pos) |> cont
        else
            simplestRational
                (newSRational dr (nr - floorL * dr), pos)
                (newSRational dl (nl - floorL * dl), pos)
                pos
                (function
                 | SRational(pn, pd), _ -> (newSRational (floorL * pn + pd) pn, pos) |> cont
                 | x -> x |> invalid (snd x) " '%s' is not a rational.")

    let sRationalize envs pos cont =
        let toExactValue =
            function
            | SRational _, _ as x -> x
            | SReal r, _ when finiteFloat r ->
                let s = (r |> sprintf "%.15f").TrimEnd('0').TrimEnd('.')

                if s.Contains(".") then
                    let parts = s.Split('.')
                    let nStr = (if parts.[0] = "0" then "" else parts.[0]) + parts.[1]
                    let n = if nStr = "" || nStr = "-" then 0I else bigint.Parse nStr
                    let d = bigint.Pow(10I, parts.[1].Length)
                    newSRational n d, pos
                else
                    try
                        newSRational (bigint.Parse s) 1I, pos
                    with _ ->
                        newSRational (bigint r) 1I, pos
            | x -> x

        function
        | [ x; e ] ->
            match toExactValue x, toExactValue e with
            | (SRational _, _), (SRational _, _) as exacts ->
                let xVal = fst exacts
                let eVal = snd exacts
                let l = subtractNumber envs pos id [ xVal; eVal ]
                let r = addNumber envs pos id [ xVal; eVal ]

                match l, r with
                | (SRational _, _), (SRational _, _) -> simplestRational l r pos cont
                | _ -> x |> cont
            | _ -> x |> cont
        | x -> x |> invalidParameter pos "'%s' invalid rationalize parameter."

    let sExp envs pos cont =
        function
        | [ SReal r, _ ] -> (r |> exp |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> (c |> System.Numerics.Complex.Exp |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> (float n / float d |> exp |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid exp parameter."

    let sLog envs pos cont =
        function
        | [ SReal r, _ ] -> (r |> log |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> (c |> System.Numerics.Complex.Log |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> (float n / float d |> log |> SReal, pos) |> cont
        | [ x; b ] ->
            let res =
                (x |> toComplex |> System.Numerics.Complex.Log)
                / (b |> toComplex |> System.Numerics.Complex.Log)

            ((if res.Imaginary = 0.0 then SReal res.Real else SComplex res), pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid log parameter."

    let sSin envs pos cont =
        function
        | [ SReal r, _ ] -> (r |> sin |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> (c |> System.Numerics.Complex.Sin |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> (float n / float d |> sin |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid sin parameter."

    let sCos envs pos cont =
        function
        | [ SReal r, _ ] -> (r |> cos |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> (c |> System.Numerics.Complex.Cos |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> (float n / float d |> cos |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid cos parameter."

    let sTan envs pos cont =
        function
        | [ SReal r, _ ] -> (r |> tan |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> (c |> System.Numerics.Complex.Tan |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> (float n / float d |> tan |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid tan parameter."

    let sAsin envs pos cont =
        function
        | [ SReal r, _ ] -> (r |> asin |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> (c |> System.Numerics.Complex.Asin |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> (float n / float d |> asin |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid asin parameter."

    let sAcos envs pos cont =
        function
        | [ SReal r, _ ] -> (r |> acos |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> (c |> System.Numerics.Complex.Acos |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> (float n / float d |> acos |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid acos parameter."

    let sAtan envs pos cont =
        function
        | [ SReal r, _ ] -> (r |> atan |> SReal, pos) |> cont
        | [ SComplex c, _ ] -> (c |> System.Numerics.Complex.Atan |> SComplex, pos) |> cont
        | [ SRational(n, d), _ ] -> (float n / float d |> atan |> SReal, pos) |> cont
        | [ y, _; x, _ ] ->
            let yVal =
                match y with
                | SReal r -> r
                | SRational(n, d) -> float n / float d
                | _ -> 0.0

            let xVal =
                match x with
                | SReal r -> r
                | SRational(n, d) -> float n / float d
                | _ -> 0.0

            (System.Math.Atan2(yVal, xVal) |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid atan parameter."

    let sSquare envs pos cont =
        function
        | [ x ] -> multiplyNumber envs pos cont [ x; x ]
        | x -> x |> invalidParameter pos "'%s' invalid square parameter."

    let sSqrt envs pos cont =
        function
        | [ SRational(n, d), _ ] when n >= 0I -> (float n / float d |> sqrt |> SReal, pos) |> cont
        | [ SRational(n, d), _ ] ->
            (System.Numerics.Complex(float n / float d, 0.0)
             |> System.Numerics.Complex.Sqrt
             |> SComplex,
             pos)
            |> cont
        | [ SReal r, _ ] when r >= 0.0 -> (r |> sqrt |> SReal, pos) |> cont
        | [ SReal r, _ ] ->
            (System.Numerics.Complex(r, 0.0) |> System.Numerics.Complex.Sqrt |> SComplex, pos)
            |> cont
        | [ SComplex c, _ ] -> (c |> System.Numerics.Complex.Sqrt |> SComplex, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid sqrt parameter."

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
            (SValues [ newSRational s 1I, pos; newSRational r 1I, pos ], pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid exact-integer-sqrt parameter."

    let sExpt envs pos cont =
        function
        | [ x; y ] ->
            match x, y with
            | (SRational(n1, d1), _), (SRational(n2, d2), _) when d1 = 1I && d2 = 1I && n2 >= 0I ->
                bigint.Pow(n1, int n2) |> fun v -> (newSRational v 1I, pos) |> cont
            | _ -> (System.Numerics.Complex.Pow(toComplex x, toComplex y) |> SComplex, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid expt parameter."

    let sMakeRectangular envs pos cont =
        function
        | [ r; i ] ->
            (System.Numerics.Complex((toComplex r).Real, (toComplex i).Real) |> SComplex, pos)
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-rectangular parameter."

    let sMakePolar envs pos cont =
        function
        | [ r; theta ] ->
            (System.Numerics.Complex.FromPolarCoordinates((toComplex r).Real, (toComplex theta).Real)
             |> SComplex,
             pos)
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-polar parameter."

    let sRealPart envs pos cont =
        function
        | [ x ] -> ((toComplex x).Real |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid real-part parameter."

    let sImagPart envs pos cont =
        function
        | [ x ] -> ((toComplex x).Imaginary |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid imag-part parameter."

    let sMagnitude envs pos cont =
        function
        | [ x ] -> ((toComplex x).Magnitude |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid magnitude parameter."

    let sAngle envs pos cont =
        function
        | [ x ] -> ((toComplex x).Phase |> SReal, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid angle parameter."

    let sInexact envs pos cont =
        function
        | [ SRational(n, d), _ ] -> (SReal(float n / float d), pos) |> cont
        | [ SReal _, _ ] as x -> x.Head |> cont
        | [ SComplex _, _ ] as x -> x.Head |> cont
        | x -> x |> invalidParameter pos "'%s' invalid inexact parameter."

    let sExact envs pos cont =
        function
        | [ SRational _, _ ] as x -> x.Head |> cont
        | [ SReal r, _ ] when finiteFloat r ->
            try
                match r |> sprintf "%.17g" |> Read.read with
                | SRational _, _ as res -> res |> cont
                | _ -> (newSRational (bigint r) 1I, pos) |> cont
            with _ ->
                (newSRational (bigint r) 1I, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid exact parameter."

    let sNumberToString envs pos cont =
        function
        | [ n ] -> (n |> Print.print |> newSString true, pos) |> cont
        | [ n; SRational(radix, d), _ ] when d = 1I ->
            match n with
            | SRational(k, d'), _ ->
                (if d' = 1I then
                     match int radix with
                     | 2 -> System.Convert.ToString(int64 k, 2)
                     | 8 -> System.Convert.ToString(int64 k, 8)
                     | 10 -> string k
                     | 16 -> System.Convert.ToString(int64 k, 16)
                     | x -> failwithf "'%d' unsupported radix in number->string.%s" x (pos |> formatPosition)
                 else
                     sprintf "%A/%A" k d'
                 |> newSString true,
                 pos)
                |> cont
            | _ -> (n |> Print.print |> newSString true, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid number->string parameter."

    let sStringToNumber envs pos cont =
        function
        | [ SString s, _ ] ->
            try
                match s.runes |> runesToString |> Read.read with
                | SRational _, _
                | SReal _, _
                | SComplex _, _ as n -> n |> cont
                | _ -> (SFalse, pos) |> cont
            with _ ->
                (SFalse, pos) |> cont
        | [ SString data, _; SRational(radix, d), _ ] when d = 1I ->
            try
                let s = data.runes |> runesToString

                let v =
                    match int radix with
                    | 2 -> System.Convert.ToInt64(s, 2) |> bigint
                    | 8 -> System.Convert.ToInt64(s, 8) |> bigint
                    | 10 -> bigint.Parse s
                    | 16 -> System.Convert.ToInt64(s, 16) |> bigint
                    | x -> failwithf "'%d' unsupported radix in string->number.%s" x (pos |> formatPosition)

                (newSRational v 1I, pos) |> cont
            with _ ->
                (SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string->number parameter."
