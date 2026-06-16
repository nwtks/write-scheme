namespace WriteScheme.Builtins

open WriteScheme
open Type

type SNumber =
    | NRational of bigint * bigint
    | NReal of float
    | NComplex of System.Numerics.Complex

module SNumber =
    let toFloat n d = float n / float d

    let nRational n d =
        normalizeRational n d |> Result.map NRational

    let ofExpr (x, _) =
        match x with
        | SRational(n, d) -> NRational(n, d) |> Ok
        | SReal r -> NReal r |> Ok
        | SComplex c -> NComplex c |> Ok
        | _ -> Error "not a number"

    let toSExpr pos =
        function
        | NRational(n, d) -> ((if n = 0I then SZero else SRational(n, d)), pos) |> Ok
        | NReal r -> (SReal r, pos) |> Ok
        | NComplex c -> (SComplex c, pos) |> Ok

    let promoteToComplex =
        function
        | NRational(n, d) -> System.Numerics.Complex(toFloat n d, 0.0)
        | NReal r -> System.Numerics.Complex(r, 0.0)
        | NComplex c -> c

    let add a b =
        match a, b with
        | NRational(n1, d1), NRational(n2, d2) -> nRational (n1 * d2 + n2 * d1) (d1 * d2)
        | NRational(n, d), NReal r -> NReal(toFloat n d + r) |> Ok
        | NReal r, NRational(n, d) -> NReal(r + toFloat n d) |> Ok
        | NReal r1, NReal r2 -> NReal(r1 + r2) |> Ok
        | _, _ -> NComplex(promoteToComplex a + promoteToComplex b) |> Ok

    let sub a b =
        match a, b with
        | NRational(n1, d1), NRational(n2, d2) -> nRational (n1 * d2 - n2 * d1) (d1 * d2)
        | NRational(n, d), NReal r -> NReal(toFloat n d - r) |> Ok
        | NReal r, NRational(n, d) -> NReal(r - toFloat n d) |> Ok
        | NReal r1, NReal r2 -> NReal(r1 - r2) |> Ok
        | _, _ -> NComplex(promoteToComplex a - promoteToComplex b) |> Ok

    let mul a b =
        match a, b with
        | NRational(n1, d1), NRational(n2, d2) -> nRational (n1 * n2) (d1 * d2)
        | NRational(n, d), NReal r -> NReal(toFloat n d * r) |> Ok
        | NReal r, NRational(n, d) -> NReal(r * toFloat n d) |> Ok
        | NReal r1, NReal r2 -> NReal(r1 * r2) |> Ok
        | _, _ -> NComplex(promoteToComplex a * promoteToComplex b) |> Ok

    let div a b =
        match a, b with
        | NRational(n1, d1), NRational(n2, d2) ->
            if n2 = 0I then
                Error "Division by zero."
            else
                nRational (n1 * d2) (d1 * n2)
        | NRational(n, d), NReal r ->
            if r = 0.0 then
                Error "Division by zero."
            else
                NReal(toFloat n d / r) |> Ok
        | NReal r, NRational(n, d) ->
            let f = toFloat n d

            if f = 0.0 then
                Error "Division by zero."
            else
                NReal(r / f) |> Ok
        | NReal r1, NReal r2 ->
            if r2 = 0.0 then
                Error "Division by zero."
            else
                NReal(r1 / r2) |> Ok
        | _, _ ->
            let c2 = promoteToComplex b

            if c2.Magnitude = 0.0 then
                Error "Division by zero."
            else
                NComplex(promoteToComplex a / c2) |> Ok
