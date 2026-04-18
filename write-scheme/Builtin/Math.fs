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
