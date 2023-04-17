module QuadraticModule

    open System

    let discriminate a b c = b * b - 4.0 * a * c

    let solveQuadratic a b c discriminant = 
        let sqrtDiscriminant = sqrt discriminant
        let x1 = (-b + sqrtDiscriminant) / (2.0 * a)
        let x2 = (-b - sqrtDiscriminant) / (2.0 * a)
        x1, x2

    let quadraticFormula a b c =
        let discriminant = discriminate a b c

        match discriminant with
        | d when d < 0.0 -> None
        | d -> Some (solveQuadratic a b c d)
