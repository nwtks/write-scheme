namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Math =
    let isNumber envs cont =
        function
        | [ SRational _ ]
        | [ SReal _ ]
        | [ SComplex _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    let isComplex envs cont =
        function
        | [ SComplex _ ]
        | [ SReal _ ]
        | [ SRational _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    let isReal envs cont =
        function
        | [ SRational _ ]
        | [ SReal _ ] -> STrue |> cont
        | [ SComplex c ] when c.Imaginary = 0.0 -> STrue |> cont
        | [ SComplex _ ] -> SFalse |> cont
        | _ -> SFalse |> cont

    let isRational envs cont =
        function
        | [ SRational _ ] -> STrue |> cont
        | [ SReal r ] when not (System.Double.IsInfinity r || System.Double.IsNaN r) -> STrue |> cont
        | [ SComplex c ] when
            c.Imaginary = 0.0
            && not (System.Double.IsInfinity c.Real || System.Double.IsNaN c.Real)
            ->
            STrue |> cont
        | _ -> SFalse |> cont

    let isInteger envs cont =
        function
        | [ SRational(_, d) ] when d = 1I -> STrue |> cont
        | [ SReal r ] when
            not (System.Double.IsInfinity r || System.Double.IsNaN r)
            && r = System.Math.Truncate r
            ->
            STrue |> cont
        | [ SComplex c ] when
            c.Imaginary = 0.0
            && not (System.Double.IsInfinity c.Real || System.Double.IsNaN c.Real)
            && c.Real = System.Math.Truncate c.Real
            ->
            STrue |> cont
        | _ -> SFalse |> cont

    let isExact envs cont =
        function
        | [ SRational _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    let isInexact envs cont =
        function
        | [ SReal _ ]
        | [ SComplex _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    let isExactInteger envs cont =
        function
        | [ SRational(_, d) ] when d = 1I -> STrue |> cont
        | _ -> SFalse |> cont

    let isFinite envs cont =
        function
        | [ SRational _ ] -> STrue |> cont
        | [ SReal r ] -> not (System.Double.IsInfinity r || System.Double.IsNaN r) |> toSBool |> cont
        | [ SComplex c ] ->
            not (
                System.Double.IsInfinity c.Real
                || System.Double.IsNaN c.Real
                || System.Double.IsInfinity c.Imaginary
                || System.Double.IsNaN c.Imaginary
            )
            |> toSBool
            |> cont
        | _ -> SFalse |> cont

    let isInfinite envs cont =
        function
        | [ SReal r ] -> System.Double.IsInfinity r |> toSBool |> cont
        | [ SComplex c ] ->
            (System.Double.IsInfinity c.Real || System.Double.IsInfinity c.Imaginary)
            |> toSBool
            |> cont
        | _ -> SFalse |> cont

    let isNaN envs cont =
        function
        | [ SReal r ] -> System.Double.IsNaN r |> toSBool |> cont
        | [ SComplex c ] ->
            (System.Double.IsNaN c.Real || System.Double.IsNaN c.Imaginary)
            |> toSBool
            |> cont
        | _ -> SFalse |> cont

    let toFloat x y = float x / float y

    let toComplex =
        function
        | SRational(x1, x2) -> System.Numerics.Complex(toFloat x1 x2, 0.0)
        | SReal x -> System.Numerics.Complex(x, 0.0)
        | SComplex c -> c
        | a -> sprintf "'%s' not number." (Print.print a) |> failwith

    let comparePred pred1 pred2 pred3 =
        function
        | SRational(a1, a2), SRational(b1, b2) -> pred1 (a1 * b2) (b1 * a2)
        | SRational(a1, a2), SReal b -> pred2 (toFloat a1 a2) b
        | SReal a, SRational(b1, b2) -> pred2 a (toFloat b1 b2)
        | SReal a, SReal b -> pred2 a b
        | SComplex _, _
        | _, SComplex _ as pair ->
            let x, y = fst pair |> toComplex, snd pair |> toComplex
            pred3 x y
        | _ -> false

    [<TailCall>]
    let rec compare pred1 pred2 pred3 n =
        function
        | [] -> STrue
        | x :: xs ->
            if comparePred pred1 pred2 pred3 (n, x) then
                compare pred1 pred2 pred3 x xs
            else
                SFalse

    let compareNumber pred1 pred2 pred3 cont =
        function
        | [] -> STrue |> cont
        | x :: xs -> xs |> compare pred1 pred2 pred3 x |> cont

    let complexReal op (x: System.Numerics.Complex) (y: System.Numerics.Complex) =
        if x.Imaginary = 0.0 && y.Imaginary = 0.0 then
            op x.Real y.Real
        else
            failwith "Ordering on complex numbers with non-zero imaginary parts is undefined."

    let equalNumber envs cont args = compareNumber (=) (=) (=) cont args

    let lessNumber envs cont args =
        compareNumber (<) (<) (complexReal (<)) cont args

    let greaterNumber envs cont args =
        compareNumber (>) (>) (complexReal (>)) cont args

    let lessEqualNumber envs cont args =
        compareNumber (<=) (<=) (complexReal (<=)) cont args

    let greaterEqualNumber envs cont args =
        compareNumber (>=) (>=) (complexReal (>=)) cont args

    let isZero envs cont =
        function
        | [ x ] -> equalNumber envs cont [ x; SZero ]
        | _ -> SFalse |> cont

    let isPositive envs cont =
        function
        | [ x ] -> greaterNumber envs cont [ x; SZero ]
        | _ -> SFalse |> cont

    let isNegative envs cont =
        function
        | [ x ] -> lessNumber envs cont [ x; SZero ]
        | _ -> SFalse |> cont

    let isOdd envs cont =
        function
        | [ SRational(n, d) ] when d = 1I -> abs n % 2I = 1I |> toSBool |> cont
        | [ SReal r ] when
            not (System.Double.IsInfinity r || System.Double.IsNaN r)
            && r = System.Math.Truncate r
            ->
            abs r % 2.0 = 1.0 |> toSBool |> cont
        | [ SComplex c ] when
            c.Imaginary = 0.0
            && not (System.Double.IsInfinity c.Real || System.Double.IsNaN c.Real)
            && c.Real = System.Math.Truncate c.Real
            ->
            abs c.Real % 2.0 = 1.0 |> toSBool |> cont
        | x -> sprintf "'odd?' requires an integer, given %s" (Print.printList x) |> failwith

    let isEven envs cont =
        function
        | [ SRational(n, d) ] when d = 1I -> n % 2I = 0I |> toSBool |> cont
        | [ SReal r ] when
            not (System.Double.IsInfinity r || System.Double.IsNaN r)
            && r = System.Math.Truncate r
            ->
            r % 2.0 = 0.0 |> toSBool |> cont
        | [ SComplex c ] when
            c.Imaginary = 0.0
            && not (System.Double.IsInfinity c.Real || System.Double.IsNaN c.Real)
            && c.Real = System.Math.Truncate c.Real
            ->
            c.Real % 2.0 = 0.0 |> toSBool |> cont
        | x -> sprintf "'even?' requires an integer, given %s" (Print.printList x) |> failwith

    let isAnyInexact args =
        args
        |> List.exists (function
            | SRational _ -> false
            | _ -> true)

    let sMax envs cont args =
        if List.isEmpty args then
            args |> invalidParameter "'max' required at least 1 argument. Given: %s"
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
                | SRational(n, d) -> SReal(float n / float d)
                | _ -> maxVal
            else
                maxVal
            |> cont

    let sMin envs cont args =
        if List.isEmpty args then
            args |> invalidParameter "'min' required at least 1 argument. Given: %s"
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
                | SRational(n, d) -> SReal(float n / float d)
                | _ -> minVal
            else
                minVal
            |> cont

    let calc op1 op2 op3 ident1 ident2 ident3 cont =
        let op x y =
            match x, y with
            | SRational(a1, a2), SRational(b1, b2) -> op1 a1 a2 b1 b2
            | SRational(a1, a2), SReal b -> op2 (toFloat a1 a2) b
            | SReal a, SRational(b1, b2) -> op2 a (toFloat b1 b2)
            | SReal a, SReal b -> op2 a b
            | SComplex _, _
            | _, SComplex _ -> op3 (toComplex x) (toComplex y)
            | a, b -> sprintf "'%s', '%s' not number." (Print.print a) (Print.print b) |> failwith

        function
        | [] -> SRational(ident1, 1I) |> cont
        | [ SRational(x1, x2) ] -> op1 ident1 1I x1 x2 |> cont
        | [ SReal x ] -> op2 ident2 x |> cont
        | [ SComplex c ] -> op3 ident3 c |> cont
        | x :: xs -> List.fold op x xs |> cont

    let addNumber envs cont args =
        calc
            (fun a1 a2 b1 b2 -> newSRational (a1 * b2 + b1 * a2) (a2 * b2))
            (fun n1 n2 -> n1 + n2 |> SReal)
            (fun c1 c2 -> c1 + c2 |> SComplex)
            0I
            0.0
            System.Numerics.Complex.Zero
            cont
            args

    let multiplyNumber envs cont args =
        calc
            (fun a1 a2 b1 b2 -> newSRational (a1 * b1) (a2 * b2))
            (fun n1 n2 -> n1 * n2 |> SReal)
            (fun c1 c2 -> c1 * c2 |> SComplex)
            1I
            1.0
            System.Numerics.Complex.One
            cont
            args

    let subtractNumber envs cont args =
        calc
            (fun a1 a2 b1 b2 -> newSRational (a1 * b2 - b1 * a2) (a2 * b2))
            (fun n1 n2 -> n1 - n2 |> SReal)
            (fun c1 c2 -> c1 - c2 |> SComplex)
            0I
            0.0
            System.Numerics.Complex.Zero
            cont
            args

    let divideNumber envs cont args =
        calc
            (fun a1 a2 b1 b2 -> newSRational (a1 * b2) (a2 * b1))
            (fun n1 n2 -> n1 / n2 |> SReal)
            (fun c1 c2 -> c1 / c2 |> SComplex)
            1I
            1.0
            System.Numerics.Complex.One
            cont
            args

    let sAbs envs cont =
        function
        | [ SRational(n, d) ] -> newSRational (abs n) d |> cont
        | [ SReal r ] -> abs r |> SReal |> cont
        | [ SComplex c ] -> c.Magnitude |> SReal |> cont
        | x -> x |> invalidParameter "'%s' invalid abs parameter."

    let truncateDiv n d = n / d, n % d

    let floorDiv (n: bigint) (d: bigint) =
        let q, r = truncateDiv n d
        if r <> 0I && n.Sign <> d.Sign then q - 1I, r + d else q, r

    let sFloorDiv envs cont =
        function
        | [ SRational(n1, d1); SRational(n2, d2) ] when d1 = 1I && d2 = 1I ->
            let q, r = floorDiv n1 n2
            SValues [ newSRational q 1I; newSRational r 1I ] |> cont
        | x -> x |> invalidParameter "'%s' invalid floor/ parameter."

    let sFloorQuotient envs cont =
        function
        | [ SRational(n1, d1); SRational(n2, d2) ] when d1 = 1I && d2 = 1I ->
            let q, _ = floorDiv n1 n2
            newSRational q 1I |> cont
        | x -> x |> invalidParameter "'%s' invalid floor-quotient parameter."

    let sFloorRemainder envs cont =
        function
        | [ SRational(n1, d1); SRational(n2, d2) ] when d1 = 1I && d2 = 1I ->
            let _, r = floorDiv n1 n2
            newSRational r 1I |> cont
        | x -> x |> invalidParameter "'%s' invalid floor-remainder parameter."

    let sTruncateDiv envs cont =
        function
        | [ SRational(n1, d1); SRational(n2, d2) ] when d1 = 1I && d2 = 1I ->
            let q, r = truncateDiv n1 n2
            SValues [ newSRational q 1I; newSRational r 1I ] |> cont
        | x -> x |> invalidParameter "'%s' invalid truncate/ parameter."

    let sTruncateQuotient envs cont =
        function
        | [ SRational(n1, d1); SRational(n2, d2) ] when d1 = 1I && d2 = 1I ->
            let q, _ = truncateDiv n1 n2
            newSRational q 1I |> cont
        | x -> x |> invalidParameter "'%s' invalid truncate-quotient parameter."

    let sTruncateRemainder envs cont =
        function
        | [ SRational(n1, d1); SRational(n2, d2) ] when d1 = 1I && d2 = 1I ->
            let _, r = truncateDiv n1 n2
            newSRational r 1I |> cont
        | x -> x |> invalidParameter "'%s' invalid truncate-remainder parameter."

    let sQuotient envs cont =
        function
        | [ SRational(n1, d1); SRational(n2, d2) ] when d1 = 1I && d2 = 1I -> newSRational (n1 / n2) 1I |> cont
        | x -> x |> invalidParameter "'%s' invalid quotient parameter."

    let sRemainder envs cont =
        function
        | [ SRational(n1, d1); SRational(n2, d2) ] when d1 = 1I && d2 = 1I -> newSRational (n1 % n2) 1I |> cont
        | x -> x |> invalidParameter "'%s' invalid remainder parameter."

    let sModulo envs cont =
        function
        | [ SRational(n1, d1); SRational(n2, d2) ] when d1 = 1I && d2 = 1I ->
            let r = n1 % n2

            if r <> 0I && n1.Sign <> n2.Sign then
                newSRational (r + n2) 1I
            else
                newSRational r 1I
            |> cont
        | x -> x |> invalidParameter "'%s' invalid modulo parameter."

    let gcd a b = bigint.GreatestCommonDivisor(a, b)

    let sGcd envs cont args =
        args
        |> List.map (function
            | SRational(n, d) when d = 1I -> n
            | x -> failwithf "gcd requires integers, given %s" (Print.print x))
        |> List.fold gcd 0I
        |> fun v -> newSRational v 1I
        |> cont

    let lcm a b =
        if a = 0I || b = 0I then 0I else abs (a * b) / gcd a b

    let sLcm envs cont args =
        args
        |> List.map (function
            | SRational(n, d) when d = 1I -> n
            | x -> failwithf "lcm requires integers, given %s" (Print.print x))
        |> List.fold lcm 1I
        |> fun v -> newSRational v 1I
        |> cont

    let sNumerator envs cont =
        function
        | [ SRational(n, _) ] -> newSRational n 1I |> cont
        | x -> x |> invalidParameter "'%s' invalid numerator parameter."

    let sDenominator envs cont =
        function
        | [ SRational(_, d) ] -> newSRational d 1I |> cont
        | x -> x |> invalidParameter "'%s' invalid denominator parameter."

    let sFloor envs cont =
        function
        | [ SRational(n, d) ] ->
            let q, r = truncateDiv n d

            if r <> 0I && n.Sign <> d.Sign then q - 1I else q
            |> fun v -> newSRational v 1I
            |> cont
        | [ SReal r ] -> floor r |> SReal |> cont
        | x -> x |> invalidParameter "'%s' invalid floor parameter."

    let sCeiling envs cont =
        function
        | [ SRational(n, d) ] ->
            let q, r = truncateDiv n d

            if r <> 0I && n.Sign = d.Sign then q + 1I else q
            |> fun v -> newSRational v 1I
            |> cont
        | [ SReal r ] -> ceil r |> SReal |> cont
        | x -> x |> invalidParameter "'%s' invalid ceiling parameter."

    let sTruncate envs cont =
        function
        | [ SRational(n, d) ] -> newSRational (n / d) 1I |> cont
        | [ SReal r ] -> truncate r |> SReal |> cont
        | x -> x |> invalidParameter "'%s' invalid truncate parameter."

    let sRound envs cont =
        function
        | [ SRational(n, d) ] ->
            let r = float n / float d
            newSRational (round r |> bigint) 1I |> cont
        | [ SReal r ] -> round r |> SReal |> cont
        | x -> x |> invalidParameter "'%s' invalid round parameter."

    [<TailCall>]
    let rec simplestRational l r cont =
        let nl, dl =
            match l with
            | SRational(n, d) -> n, d
            | _ -> failwith "simplestRational: l not rational"

        let nr, dr =
            match r with
            | SRational(n, d) -> n, d
            | _ -> failwith "simplestRational: r not rational"

        let floorL = if nl >= 0I then nl / dl else (nl - dl + 1I) / dl
        let ceilL = if nl % dl = 0I then floorL else floorL + 1I
        let floorR = if nr >= 0I then nr / dr else (nr - dr + 1I) / dr

        if ceilL <= floorR then
            if ceilL > 0I then newSRational ceilL 1I |> cont
            elif floorR < 0I then newSRational floorR 1I |> cont
            else newSRational 0I 1I |> cont
        else
            simplestRational (newSRational dr (nr - floorL * dr)) (newSRational dl (nl - floorL * dl)) (function
                | SRational(pn, pd) -> newSRational (floorL * pn + pd) pn |> cont
                | _ -> failwith "simplestRational: unexpected result")

    let sRationalize envs cont =
        let toExactValue x =
            match x with
            | SRational _ -> x
            | SReal r ->
                if System.Double.IsInfinity r || System.Double.IsNaN r then
                    x
                else
                    let s =
                        r.ToString("F15", System.Globalization.CultureInfo.InvariantCulture).TrimEnd('0').TrimEnd('.')

                    if s.Contains(".") then
                        let parts = s.Split('.')
                        let nStr = (if parts.[0] = "0" then "" else parts.[0]) + parts.[1]
                        let n = if nStr = "" || nStr = "-" then 0I else bigint.Parse nStr
                        let d = bigint.Pow(10I, parts.[1].Length)
                        newSRational n d
                    else
                        try
                            newSRational (bigint.Parse s) 1I
                        with _ ->
                            newSRational (bigint r) 1I
            | _ -> x

        function
        | [ x; e ] ->
            match toExactValue x, toExactValue e with
            | SRational _, SRational _ as exacts ->
                let xVal = fst exacts
                let eVal = snd exacts
                let l = subtractNumber envs id [ xVal; eVal ]
                let r = addNumber envs id [ xVal; eVal ]

                match l, r with
                | SRational _, SRational _ -> simplestRational l r cont
                | _ -> x |> cont
            | _ -> x |> cont
        | x -> x |> invalidParameter "'%s' invalid rationalize parameter."

    let sExp envs cont =
        function
        | [ SReal r ] -> exp r |> SReal |> cont
        | [ SComplex c ] -> System.Numerics.Complex.Exp c |> SComplex |> cont
        | [ SRational(n, d) ] -> exp (float n / float d) |> SReal |> cont
        | x -> x |> invalidParameter "'%s' invalid exp parameter."

    let sLog envs cont =
        function
        | [ SReal r ] -> log r |> SReal |> cont
        | [ SComplex c ] -> System.Numerics.Complex.Log c |> SComplex |> cont
        | [ SRational(n, d) ] -> log (float n / float d) |> SReal |> cont
        | [ x; base_ ] ->
            let xc = toComplex x
            let bc = toComplex base_
            let res = System.Numerics.Complex.Log xc / System.Numerics.Complex.Log bc

            if res.Imaginary = 0.0 then SReal res.Real else SComplex res
            |> cont
        | x -> x |> invalidParameter "'%s' invalid log parameter."

    let sSin envs cont =
        function
        | [ SReal r ] -> sin r |> SReal |> cont
        | [ SComplex c ] -> System.Numerics.Complex.Sin c |> SComplex |> cont
        | [ SRational(n, d) ] -> sin (float n / float d) |> SReal |> cont
        | x -> x |> invalidParameter "'%s' invalid sin parameter."

    let sCos envs cont =
        function
        | [ SReal r ] -> cos r |> SReal |> cont
        | [ SComplex c ] -> System.Numerics.Complex.Cos c |> SComplex |> cont
        | [ SRational(n, d) ] -> cos (float n / float d) |> SReal |> cont
        | x -> x |> invalidParameter "'%s' invalid cos parameter."

    let sTan envs cont =
        function
        | [ SReal r ] -> tan r |> SReal |> cont
        | [ SComplex c ] -> System.Numerics.Complex.Tan c |> SComplex |> cont
        | [ SRational(n, d) ] -> tan (float n / float d) |> SReal |> cont
        | x -> x |> invalidParameter "'%s' invalid tan parameter."

    let sAsin envs cont =
        function
        | [ SReal r ] -> asin r |> SReal |> cont
        | [ SComplex c ] -> System.Numerics.Complex.Asin c |> SComplex |> cont
        | [ SRational(n, d) ] -> asin (float n / float d) |> SReal |> cont
        | x -> x |> invalidParameter "'%s' invalid asin parameter."

    let sAcos envs cont =
        function
        | [ SReal r ] -> acos r |> SReal |> cont
        | [ SComplex c ] -> System.Numerics.Complex.Acos c |> SComplex |> cont
        | [ SRational(n, d) ] -> acos (float n / float d) |> SReal |> cont
        | x -> x |> invalidParameter "'%s' invalid acos parameter."

    let sAtan envs cont =
        function
        | [ SReal r ] -> atan r |> SReal |> cont
        | [ SComplex c ] -> System.Numerics.Complex.Atan c |> SComplex |> cont
        | [ SRational(n, d) ] -> atan (float n / float d) |> SReal |> cont
        | [ y; x ] ->
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

            System.Math.Atan2(yVal, xVal) |> SReal |> cont
        | x -> x |> invalidParameter "'%s' invalid atan parameter."

    let sSquare envs cont =
        function
        | [ x ] -> multiplyNumber envs cont [ x; x ]
        | x -> x |> invalidParameter "'%s' invalid square parameter."

    let sSqrt envs cont =
        function
        | [ SRational(n, d) ] when n >= 0I -> sqrt (float n / float d) |> SReal |> cont
        | [ SRational(n, d) ] ->
            System.Numerics.Complex.Sqrt(System.Numerics.Complex(float n / float d, 0.0))
            |> SComplex
            |> cont
        | [ SReal r ] when r >= 0.0 -> sqrt r |> SReal |> cont
        | [ SReal r ] ->
            System.Numerics.Complex.Sqrt(System.Numerics.Complex(r, 0.0))
            |> SComplex
            |> cont
        | [ SComplex c ] -> System.Numerics.Complex.Sqrt c |> SComplex |> cont
        | x -> x |> invalidParameter "'%s' invalid sqrt parameter."

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

    let sExactIntegerSqrt envs cont =
        function
        | [ SRational(k, d) ] when d = 1I && k >= 0I ->
            let s = if k < 2I then k else bigintSqrt 1I (k + 1I) k
            let r = k - s * s
            SValues [ newSRational s 1I; newSRational r 1I ] |> cont
        | x -> x |> invalidParameter "'%s' invalid exact-integer-sqrt parameter."

    let sExpt envs cont =
        function
        | [ x; y ] ->
            match x, y with
            | SRational(n1, d1), SRational(n2, d2) when d1 = 1I && d2 = 1I && n2 >= 0I ->
                bigint.Pow(n1, int n2) |> fun v -> newSRational v 1I |> cont
            | _ ->
                let xc = toComplex x
                let yc = toComplex y
                System.Numerics.Complex.Pow(xc, yc) |> SComplex |> cont
        | x -> x |> invalidParameter "'%s' invalid expt parameter."

    let sMakeRectangular envs cont =
        function
        | [ r; i ] ->
            System.Numerics.Complex((toComplex r).Real, (toComplex i).Real)
            |> SComplex
            |> cont
        | args ->
            sprintf "'make-rectangular' required 2 arguments, but %d" args.Length
            |> failwith

    let sMakePolar envs cont =
        function
        | [ r; theta ] ->
            System.Numerics.Complex.FromPolarCoordinates((toComplex r).Real, (toComplex theta).Real)
            |> SComplex
            |> cont
        | args -> sprintf "'make-polar' required 2 arguments, but %d" args.Length |> failwith

    let sRealPart envs cont =
        function
        | [ x ] -> (toComplex x).Real |> SReal |> cont
        | args -> sprintf "'real-part' required 1 argument, but %d" args.Length |> failwith

    let sImagPart envs cont =
        function
        | [ x ] -> (toComplex x).Imaginary |> SReal |> cont
        | args -> sprintf "'imag-part' required 1 argument, but %d" args.Length |> failwith

    let sMagnitude envs cont =
        function
        | [ x ] -> (toComplex x).Magnitude |> SReal |> cont
        | args -> sprintf "'magnitude' required 1 argument, but %d" args.Length |> failwith

    let sAngle envs cont =
        function
        | [ x ] -> (toComplex x).Phase |> SReal |> cont
        | args -> sprintf "'angle' required 1 argument, but %d" args.Length |> failwith

    let sInexact envs cont =
        function
        | [ SRational(n, d) ] -> SReal(float n / float d) |> cont
        | [ SReal _ ] as x -> x.Head |> cont
        | [ SComplex _ ] as x -> x.Head |> cont
        | x -> x |> invalidParameter "'%s' invalid inexact parameter."

    let sExact envs cont =
        function
        | [ SRational _ ] as x -> x.Head |> cont
        | [ SReal r ] ->
            if System.Double.IsInfinity r || System.Double.IsNaN r then
                failwith "exact: cannot convert infinity or NaN to exact"

            let s = r.ToString("G17", System.Globalization.CultureInfo.InvariantCulture)

            try
                match Read.read s with
                | SRational _ as res -> res |> cont
                | _ -> newSRational (bigint r) 1I |> cont
            with _ ->
                newSRational (bigint r) 1I |> cont
        | x -> x |> invalidParameter "'%s' invalid exact parameter."

    let sNumberToString envs cont =
        function
        | [ n ] -> n |> Print.print |> SString |> cont
        | [ n; SRational(radix, d) ] when d = 1I ->
            match n with
            | SRational(v, vd) when vd = 1I ->
                match int radix with
                | 2 -> System.Convert.ToString(int64 v, 2) |> SString |> cont
                | 8 -> System.Convert.ToString(int64 v, 8) |> SString |> cont
                | 10 -> string v |> SString |> cont
                | 16 -> System.Convert.ToString(int64 v, 16) |> SString |> cont
                | _ -> failwith "number->string: unsupported radix"
            | _ -> n |> Print.print |> SString |> cont
        | x -> x |> invalidParameter "'%s' invalid number->string parameter."

    let sStringToNumber envs cont =
        function
        | [ SString s ] ->
            try
                match Read.read s with
                | SRational _
                | SReal _
                | SComplex _ as n -> n |> cont
                | _ -> SFalse |> cont
            with _ ->
                SFalse |> cont
        | [ SString s; SRational(radix, d) ] when d = 1I ->
            try
                let v =
                    match int radix with
                    | 2 -> System.Convert.ToInt64(s, 2) |> bigint
                    | 8 -> System.Convert.ToInt64(s, 8) |> bigint
                    | 10 -> bigint.Parse s
                    | 16 -> System.Convert.ToInt64(s, 16) |> bigint
                    | _ -> failwith "unsupported radix"

                newSRational v 1I |> cont
            with _ ->
                SFalse |> cont
        | x -> x |> invalidParameter "'%s' invalid string->number parameter."
