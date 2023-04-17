module LazyEvaluationModule

    open System.Collections.Generic

    let lazyIf condition (lazyTrue: Lazy<_>) (lazyFalse: Lazy<_>) =
        if condition then lazyTrue.Value else lazyFalse.Value

    // let lazyExp1 = lazy (printfn "True branch"; 42)
    // let lazyExp2 = lazy (printfn "False branch"; -42)

    // let result = lazyIf true lazyExp1 lazyExp2
    // printfn "Result: %d" result

    let lazyMatch value (patterns: ('a -> bool) list) (lazyResults: Lazy<'b> list) =
        let predicateIndex = patterns |> List.tryFindIndex (fun pred -> pred value)
        match predicateIndex with
        | Some index -> (lazyResults.[index]).Force
        | None -> failwith "No matching pattern found"

    // let input = 1
    // let patterns = [
    //     (fun x -> x = 1)
    //     (fun x -> x = 2)
    // ]
    // let lazyResults = [
    //     lazy (printfn "First pattern"; "One")
    //     lazy (printfn "Second pattern"; "Two")
    // ]

    // let result = lazyMatch input patterns lazyResults
    // printfn "Result: %s" result

    let rec memoize (f: 'a -> 'b) =
        let cache = Dictionary<'a, 'b>(HashIdentity.Structural)
        fun x ->
            match cache.TryGetValue x with
            | (true, value) -> value
            | (false, _) ->
                let value = f x
                cache.[x] <- value
                value

    let rec lazyFibonacci n =
        if n <= 1 then n
        else lazyFibonacci (n - 1) + lazyFibonacci (n - 2)

    // let memoizedLazyFibonacci = memoize lazyFibonacci
    // let result = memoizedLazyFibonacci 10
    // printfn "Fibonacci(10): %d" result
