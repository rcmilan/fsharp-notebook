module LazyEvaluationModule

    open System.Collections.Generic

    // The lazyIf function takes a condition and two lazy expressions
    let lazyIf condition (lazyTrue: Lazy<_>) (lazyFalse: Lazy<_>) =
        // If the condition is true, it returns the value of the first lazy expression
        // Otherwise, it returns the value of the second lazy expression
        if condition then lazyTrue.Value else lazyFalse.Value

    // The lazyMatch function takes a value, a list of patterns, and a list of lazy expressions
    let lazyMatch value (patterns: ('a -> bool) list) (lazyResults: Lazy<'b> list) =
        // Finds the index of the first pattern that evaluates to true
        let predicateIndex = patterns |> List.tryFindIndex (fun pred -> pred value)
        // If a matching pattern is found, it returns the value of the corresponding lazy expression
        match predicateIndex with
        | Some index -> (lazyResults.[index]).Value
        | None -> failwith "No matching pattern found" // If no matching pattern is found, it raises an exception

    // The memoize function is a higher-order function that takes a function and returns a memoized version of that function
    let rec memoize (f: 'a -> 'b) =
        // Creates a cache using a dictionary with structural equality for keys
        let cache = Dictionary<'a, 'b>(HashIdentity.Structural)
        // The memoized function
        fun x ->
            // Tries to find the value in the cache for the given input
            match cache.TryGetValue x with
            | (true, value) -> value // If the value is found in the cache, it returns the cached value
            | (false, _) ->
                // If the value is not found in the cache, it computes the value and stores it in the cache
                let value = f x
                cache.[x] <- value
                value // Returns the computed value

    // A naive implementation of the Fibonacci sequence that does not use lazy evaluation or memoization
    let rec lazyFibonacci n =
        // Base case: if n is 0 or 1, return n
        if n <= 1 then
            n
        // Recursive case: return the sum of the two previous Fibonacci numbers
        else
            lazyFibonacci (n - 1) + lazyFibonacci (n - 2)
