module FunctionModule
    let calculateSum x y =
        x + y

    let memoizationSum z =
        calculateSum z 10

    let rec recursiveSum x =
        match x with
        | 0 | 100 -> x
        | n -> calculateSum x 1 |> recursiveSum