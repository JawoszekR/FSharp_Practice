// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let time f =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let res = f()
    let finish = sw.Stop()
    (res, sw.Elapsed.TotalMilliseconds |> sprintf "%f ms")

module Memoization =
    let fib = 
        let lookasideTable = new System.Collections.Generic.Dictionary<int, int>()
        let rec fibChcecked n =
            if lookasideTable.ContainsKey(n) then lookasideTable.[n]
            else if n <= 2 then 1
            else let res = fibChcecked (n-1) + fibChcecked (n-2)
                 lookasideTable.Add(n, res)
                 res
        fun n -> fibChcecked n

    let res1 = time(fun () -> fib 30)
    let res2 = time(fun () -> fib 30)
    let res3 = time(fun () -> fib 30)
    let res4 = time(fun () -> fib 30)

    type Table<'T, 'U> =
        abstract Item : 'T -> 'U with get
        abstract Discard : unit -> unit

    let memoizeAndPermitDiscard f =
        let lookasideTable = new System.Collections.Generic.Dictionary<_, _>(HashIdentity.Structural) 
        {new Table<'T, 'U> with 
                member t.Item
                    with get(n) = 
                        if lookasideTable.ContainsKey(n) then
                            lookasideTable.[n]
                        else 
                            let res = f n
                            lookasideTable.Add(n, res)
                            res
                member t.Discard() =
                    lookasideTable.Clear()}

    #nowarn "40"

    let rec fib2 = 
        memoizeAndPermitDiscard ( fun n -> 
            printfn "calc for %i" n
            if n <= 2 then 1 else fib2.[n-1] + fib2.[n-2])

    let res12 = time(fun () -> fib2.[30])
    let res22 = time(fun () -> fib2.[30])
    let res32 = time(fun () -> fib2.[30])
    let res42 = time(fun () -> fib2.[30])