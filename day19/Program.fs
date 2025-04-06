module day19

open System.Diagnostics
open System.Collections.Generic
open NUnit.Framework
open FsUnit

let part1 ((patterns, designs): string seq * string seq) =
    // Convert patterns to array for a faster lookup
    let patternsArray = patterns |> Seq.toArray
    
    // Function to process each design
    let processDesign (design: string) =
        // Use memoization to avoid recalculating results
        let memo = Dictionary<int, bool>()
        
        let rec canConstruct pos =
            // If we've reached the end of the design, we've found a valid construction
            if pos = design.Length then 
                true
            // If we've already computed this position, return the cached result
            elif memo.ContainsKey(pos) then
                memo[pos]
            else
                // Helper function to check patterns in a tail-recursive manner
                let rec tryPatterns patternIndex =
                    if patternIndex >= patternsArray.Length then
                        false
                    else
                        let pattern = patternsArray[patternIndex]
                        if pos + pattern.Length <= design.Length && 
                           design.Substring(pos, pattern.Length) = pattern && 
                           canConstruct (pos + pattern.Length) then
                            true
                        else
                            tryPatterns (patternIndex + 1)
                
                // Try all patterns starting from the first one
                let result = tryPatterns 0
                
                // Cache the result
                memo[pos] <- result
                result
        
        // Start processing from the beginning of the design
        canConstruct 0
        
    designs |> Seq.filter processDesign |> Seq.length

let part2 ((patterns, designs): string seq * string seq) =
    // Convert patterns to array for a faster lookup
    let patternsArray = patterns |> Seq.toArray
    
    // Process each design
    let processDesign (design: string) =
        // Memoization table to avoid recalculating results
        let memo = Dictionary<int, int64>()
        
        let rec countWays pos =
            // If we've reached the end of the design, we've found one valid way
            if pos = design.Length then
                1L
            // If we've already computed this position, return the cached result 
            elif memo.ContainsKey(pos) then
                memo[pos]
            else
                // Count ways for each pattern
                let mutable ways = 0L
                for pattern in patternsArray do
                    if pos + pattern.Length <= design.Length && 
                       design.Substring(pos, pattern.Length) = pattern then
                        ways <- ways + countWays (pos + pattern.Length)
                
                // Cache the result
                memo[pos] <- ways
                ways
        
        // Start counting from the beginning
        countWays 0
    
    designs |> Seq.sumBy processDesign
    

/// <summary>
/// Parses the input string into patterns and designs
/// </summary>
/// <param name="input">Raw input string with patterns on first line and designs on subsequent lines</param>
/// <returns>A tuple of (patterns sequence, designs sequence)</returns>
let parse (input: string) =
    // Split input into lines and filter out empty lines
    let lines = 
        input.Split([|'\n'|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s -> s.Trim())
        |> Array.filter (fun s -> s <> "")
    
    // Pattern matching on the line array
    match lines with
    | [||] -> failwith "Invalid input: Empty input"
    | lines when lines.Length < 2 -> 
        // Handle special case where all patterns and designs might be on one line
        match lines[0].Split([|'\n'; '\r'|], System.StringSplitOptions.RemoveEmptyEntries) with
        | parts when parts.Length >= 2 ->
            // Parse patterns - split by comma and trim
            let patterns = 
                parts[0].Split([|','|], System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.map (fun s -> s.Trim())
                |> Array.filter (fun s -> s <> "")
            
            // Parse designs - take remaining parts
            let designs =
                parts[1..]
                |> Array.map (fun s -> s.Trim())
                |> Array.filter (fun s -> s <> "")
            
            patterns, designs
        | _ -> failwith "Invalid input format: Cannot separate patterns and designs"
    | lines ->
        // Parse patterns from the first line-split by comma and trim
        let patterns = 
            lines[0].Split([|','|], System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun s -> s.Trim())
            |> Array.filter (fun s -> s <> "")
        
        // Parse designs from remaining lines
        let designs = 
            lines[1..]
            |> Array.map (fun s -> s.Trim())
            |> Array.filter (fun s -> s <> "")
        
        patterns, designs

module Example =
    let input =
        "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb"

    [<Test>]
    let testPart1 () = parse input |> part1 |> should equal 6

    [<Test>]
    let testPart2 () =
        parse input |> part2 |> should equal 16L

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    printfn $"Input length: %d{input.Length}" // Debug line
    
    let patterns, designs = parse input
    printfn $"Parsed %d{Seq.length patterns} patterns and %d{Seq.length designs} designs" // Debug line
    
    let stopwatch = Stopwatch.StartNew()
    
    let part1Result = part1 (patterns, designs)
    printfn $"Part 1: %d{part1Result}"
    
    let part2Result = part2 (patterns, designs)
    printfn $"Part 2: %d{part2Result}"
    
    stopwatch.Stop()
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"
    
    0
