module ConditionalModule
    let checkGreater (x : int , y : int) =
        if x = y then "equal"
        elif x < y then $"{x} is less than {y}"
        else $"{x} is greater than {y}"

    let matchGreater (x : int , y : int) =
        match x with
        | x when x = y -> "equal"
        | x when x < y -> $"{x} is less than {y}"
        | _ ->  $"{x} is greater than {y}"