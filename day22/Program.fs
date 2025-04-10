module day22

open System.Diagnostics
open NUnit.Framework
open FsUnit


let mix value secret = value ^^^ secret

let prune secret = secret % 16777216L

let next secret =
    let secret = secret |> mix (secret * 64L) |> prune
    let secret = secret |> mix (secret / 32L) |> prune
    secret |> mix (secret * 2048L) |> prune



let part1 (initialSecrets: int64[]) =
    // Pre-allocate the array with zeros - more efficient than copying
    let mutable sum = 0L

    // Compute the sequence in place
    for i = 0 to initialSecrets.Length - 1 do
        let mutable current = initialSecrets[i]

        for _ = 1 to 2000 do
            current <- next current

        sum <- sum + current

    sum


let part2 (initialSecrets: int64[]) =
    // Create sequences of price digits (0-9) for each initial secret
    let sequences =
        initialSecrets
        |> Array.Parallel.map (fun initial ->
            let numbers = Array.zeroCreate 2001
            numbers[0] <- initial % 10L

            // Compute the sequence in place
            let mutable current = initial

            for i in 1..2000 do
                current <- next current
                numbers[i] <- current % 10L

            numbers)

    let changes =
        sequences
        |> Array.Parallel.map (fun seq -> Array.init 2000 (fun i -> seq[i + 1] - seq[i]))

    // Use tuples instead of arrays for patterns (more efficient)
    let mutable patternMap = Map.empty<int64 * int64 * int64 * int64, int64>

    // Process each buyer's changes
    for buyerIdx = 0 to changes.Length - 1 do
        let mutable seenPatterns = Set.empty

        // Find patterns
        for i = 0 to changes[buyerIdx].Length - 4 do
            let pattern =
                changes[buyerIdx].[i],
                changes[buyerIdx].[i + 1],
                changes[buyerIdx].[i + 2],
                changes[buyerIdx].[i + 3]

            if not (seenPatterns.Contains pattern) then
                let nextPrice = sequences[buyerIdx].[i + 4]

                patternMap <-
                    match Map.tryFind pattern patternMap with
                    | None -> Map.add pattern nextPrice patternMap
                    | Some existing -> Map.add pattern (existing + nextPrice) patternMap

                seenPatterns <- Set.add pattern seenPatterns

    // Find maximum sum
    patternMap |> Map.values |> Seq.max



let parse (input: string) =
    input.Split("\n") |> Array.map (fun line -> line.TrimEnd() |> int64)


module Example =
    let input1 =
        "1
10
100
2024"

    let input2 =
        "1
2
3
2024"

    [<Test>]
    let testPart1 () =
        parse input1 |> part1 |> should equal 37327623L

    [<Test>]
    let testPart2 () =
        parse input2 |> part2 |> should equal 23L



[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    printfn $"Input length: %d{input.Length}"

    let initialSecrets = parse input |> Array.ofSeq // Convert to array once
    printfn $"Number of initial secrets: %d{initialSecrets.Length}"


    let stopwatch = Stopwatch()
    stopwatch.Start()

    initialSecrets |> part1 |> printfn "Part 1: %d"
    initialSecrets |> part2 |> printfn "Part 2: %d"

    stopwatch.Stop()

    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"

    0
