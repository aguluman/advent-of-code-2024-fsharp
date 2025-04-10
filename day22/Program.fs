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

let nthNext n secret =
    let rec loop remainingSteps currentSecret =
        if remainingSteps = 0 then
            currentSecret
        else
            loop (remainingSteps - 1) (next currentSecret)

    loop n secret

let part1 (initialSecrets: int64 seq) =
    initialSecrets |> Seq.sumBy (nthNext 2000)


let part2 (initialSecrets: int64[]) =
    // Precompute 2001 secrets for each buyer (0 through 2000)
    printfn $"Precomputing secrets for %d{initialSecrets.Length} buyers..."
        
    // Create sequences of price digits (0-9) for each initial secret
    let sequences = 
        initialSecrets 
        |> Array.Parallel.map (fun initial ->
            Array.init 2001 (fun i -> 
                if i = 0 then initial % 10L
                else nthNext i initial % 10L))
    
    // Calculate price changes between consecutive numbers
    let changes =
        sequences
        |> Array.Parallel.map (fun seq ->
            seq
            |> Array.windowed 2
            |> Array.map (fun window -> window.[1] - window.[0]))
    
    // Find patterns of 4 consecutive changes and their following prices
    let mutable fourPatternMap = Map.empty
    
    // Process each buyer's changes
    for buyerIdx = 0 to changes.Length - 1 do
        let mutable addedPatterns = Set.empty
        
        // Look for patterns starting at each position
        for startIdx = 0 to changes.[buyerIdx].Length - 4 do
            let pattern = [|
                changes.[buyerIdx].[startIdx]
                changes.[buyerIdx].[startIdx + 1]
                changes.[buyerIdx].[startIdx + 2] 
                changes.[buyerIdx].[startIdx + 3]
            |]
            
            let nextPrice = sequences.[buyerIdx].[startIdx + 4]
            
            // Add or update pattern in map
            if not (addedPatterns.Contains pattern) then
                fourPatternMap <- 
                    match Map.tryFind pattern fourPatternMap with
                    | None -> Map.add pattern nextPrice fourPatternMap
                    | Some existing -> Map.add pattern (existing + nextPrice) fourPatternMap
                addedPatterns <- Set.add pattern addedPatterns
    
    // Find maximum price sum for any pattern
    fourPatternMap
    |> Map.values
    |> Seq.max




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

    let initialSecrets = parse input
    printfn $"Number of initial secrets: %d{initialSecrets.Length}"


    let stopwatch = Stopwatch()
    stopwatch.Start()

    initialSecrets |> part1 |> printfn "Part 1: %d"
    initialSecrets |> part2 |> printfn "Part 2: %d"

    stopwatch.Stop()

    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"

    0
