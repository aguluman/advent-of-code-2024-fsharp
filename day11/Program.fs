module Day11

open Xunit
open FsUnit.Xunit
open System.Diagnostics



/// <summary>
/// Transforms a stone number according to specific rules:
/// - If stone is 0, returns [1]
/// - If stone's digit length is even, splits it into two equal parts
/// - If stone's digit length is odd, multiplies it by 2024
/// </summary>
/// <param name="stoneNumber">The input stone number to transform</param>
/// <returns>List of transformed stone numbers</returns>
let blink stoneNumber = 
    if stoneNumber = 0L then
        [ 1L ]
    else 
        let numberAsString = stoneNumber.ToString()

        if numberAsString.Length % 2 = 0 then
            let midPoint = numberAsString.Length / 2
            [int64 numberAsString[.. (midPoint - 1)]; int64 numberAsString[midPoint ..]]
        else 
            [ stoneNumber * 2024L ]



/// <summary>
/// Processes stones for 25 iterations using the transformation rules
/// </summary>
/// <param name="initialStones">Initial sequence of stone numbers</param>
/// <returns>Total count of stones after 25 iterations</returns>
let part1 (initialStones: int64 seq) =
    (List.ofSeq initialStones, [ 1..25 ])
    ||> List.fold (fun accumulatedStones _ -> accumulatedStones |> List.collect blink)
    |> List.length



/// <summary>
/// Processes stones for 75 iterations while tracking stone count frequencies
/// </summary>
/// <param name="initialStones">Initial sequence of stone numbers</param>
/// <returns>Sum of all stone frequencies after 75 iterations</returns>
let part2 (initialStones: int64 seq) =
    let stoneFrequencies = initialStones |> Seq.map (fun x -> (x, 1L)) |> Map.ofSeq

    (stoneFrequencies, [ 1..75 ])
    ||> List.fold (fun currentFrequencies _ -> 
        (Map.empty, currentFrequencies)
        ||> Map.fold (fun newFrequencies originalStone frequency -> 
            (newFrequencies, blink originalStone)
            ||> List.fold (fun updatedFrequencies transformedStone -> 
                let newFrequency = frequency + (updatedFrequencies |> Map.tryFind transformedStone |> Option.defaultValue 0L)
                updatedFrequencies |> Map.add transformedStone newFrequency)))
    |> Map.values
    |> Seq.sum

/// <summary>
/// Parses space-separated string input into a sequence of int64 numbers
/// </summary>
/// <param name="inputText">Space-separated string of numbers</param>
/// <returns>Sequence of parsed int64 numbers</returns>
let parse (inputText: string) = 
    inputText.Split(" ") |> Seq.map int64

// Test module
module Example =
    let sampleInput = "125 17"

    [<Fact>]
    let testPart1 () =
        parse sampleInput |> part1 |> should equal 55312



let main _ = 
    let inputText = stdin.ReadToEnd().TrimEnd()
    let initialStones = parse inputText

    let executionTimer = Stopwatch.StartNew()

    initialStones |> part1 |> printfn "Part 1: %d"
    initialStones |> part2 |> printfn "Part 2: %d"

    executionTimer.Stop()
    printfn $"Elapsed time: %.4f{executionTimer.Elapsed.TotalSeconds} seconds"

    0