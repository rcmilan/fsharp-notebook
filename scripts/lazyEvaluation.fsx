module LazyEvaluationModule

    open System.Collections.Generic

    let lazyIf condition (lazyTrue: Lazy<_>) (lazyFalse: Lazy<_>) =
        if condition then lazyTrue.Value else lazyFalse.Value

    let lazyMatch value (patterns: ('a -> bool) list) (lazyResults: Lazy<'b> list) =
        let predicateIndex = patterns |> List.tryFindIndex (fun pred -> pred value)

        match predicateIndex with
        | Some index -> (lazyResults.[index]).Value
        | None -> failwith "No matching pattern found"

    let memoize (f: 'a -> 'b) =
        let cache = Dictionary<'a, Lazy<'b>>(HashIdentity.Structural)

        fun x ->
            match cache.TryGetValue x with
            | (true, value) -> value.Value
            | (false, _) ->
                let value = f x
                cache.[x] <- lazy value
                value

    let rec lazyFibonacci n =
        let memoizedFib = memoize fibonacciInner
        memoizedFib n

    and fibonacciInner n =
        if n <= 1 then n
        else lazyFibonacci (n - 1) + lazyFibonacci (n - 2)
