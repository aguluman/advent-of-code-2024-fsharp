/// Day 18: RAM Run
///
/// <summary>
/// This module implements a solution to navigate through a corrupted memory space
/// where bytes are falling and blocking paths.
/// The challenge involves finding the
/// shortest path through a grid and determining the first byte that makes the exit
/// unreachable.
/// </summary>
///
/// <remarks>
/// The memory space is represented as a grid where:
/// - (0,0) is the starting position (top-left corner)
/// - (gridSize, gridSize) is the exit position (bottom-right corner)
/// - Corrupted memory positions cannot be entered
/// </remarks>
module day18

open System.Diagnostics
open System.Collections.Generic
open System.Threading.Tasks
open NUnit.Framework
open FsUnit

/// <summary>Defines the four possible movement directions: up, left, down, right</summary>
let directions = [|(-1, 0); (0, -1); (1, 0); (0, 1)|]

/// <summary>
/// Creates a memory grid representation and finds the shortest path from start to exit
/// </summary>
///
/// <remarks>
/// Uses Breadth-First Search to efficiently find the shortest path:
/// 1. Start at position (0,0) with distance 0
/// 2.
/// Explore adjacent positions in all four directions
/// 3. For each reachable uncorrupted position, record distance
/// 4. Continue until BFS reaches the destination or explores all reachable positions
/// </remarks>
///
/// <param name="gridSize">Size of the memory grid (gridSize+1 x gridSize+1)</param>
/// <param name="corruptedPositions">Coordinates (x,y) of corrupted memory locations</param>
/// <returns>Some distance if a path exists, None if destination is unreachable</returns>
let findShortestPath gridSize corruptedPositions = 
    // Create a grid representation of memory space
    let memoryGrid = Array2D.create (gridSize+1) (gridSize+1) false
    for posX, posY in corruptedPositions do
        if posX >= 0 && posX <= gridSize && posY >= 0 && posY <= gridSize then
            memoryGrid[posY, posX] <- true
    
    // Use mutable data structures for BFS
    let visitQueue = Queue<int * int>()
    let distanceMap = Dictionary<int * int, int>()
    
    // Initialize with the starting position
    visitQueue.Enqueue((0, 0))
    distanceMap[(0, 0)] <- 0
    
    // Early termination flag
    let mutable exitReached = false
    
    // BFS implementation with early termination
    while not exitReached && visitQueue.Count > 0 do
        let currentX, currentY = visitQueue.Dequeue()
        let currentDistance = distanceMap[(currentX, currentY)]
        
        // Check if we've reached the exit
        if (currentX, currentY) = (gridSize, gridSize) then
            exitReached <- true
        else
            // Check all four directions
            for directionIndex = 0 to 3 do
                let directionX, directionY = directions[directionIndex]
                let nextX, nextY = currentX + directionX, currentY + directionY
                
                // Check the bounds, and if the position is valid (not corrupted and not visited)
                if nextX >= 0 && nextX <= gridSize && 
                   nextY >= 0 && nextY <= gridSize && 
                   not memoryGrid[nextY, nextX] && 
                   not (distanceMap.ContainsKey(nextX, nextY)) then
                    
                    visitQueue.Enqueue((nextX, nextY))
                    distanceMap[(nextX, nextY)] <- currentDistance + 1
    
    // Return the shortest path length if found
    if distanceMap.ContainsKey((gridSize, gridSize)) then 
        Some distanceMap[(gridSize, gridSize)] 
    else 
        None

/// <summary>
/// Calculates the minimum steps needed to reach the exit after a specific number of bytes have fallen
/// </summary>
///
/// <param name="gridSize">Size of the memory grid</param>
/// <param name="byteCount">Number of bytes to consider from the sequence</param>
/// <param name="bytePositions">Sequence of (x,y) coordinates where bytes will fall</param>
/// <returns>The minimum number of steps to reach the exit</returns>
let calculateMinSteps ((gridSize, byteCount, bytePositions): int * int * (int * int) seq) =
    findShortestPath gridSize (Seq.take byteCount bytePositions) |> Option.get

/// <summary>
/// Finds the first byte that makes the exit unreachable from the starting position
/// </summary>
///
/// <remarks>
/// Uses binary search to efficiently find the precise byte:
/// 1. Start with the full list of positions
/// 2. Use binary search to narrow down the critical position
/// 3. When found, return the exact byte coordinates
/// </remarks>
///
/// <param name="gridSize">Size of the memory grid</param>
/// <param name="bytePositions">Sequence of (x,y) coordinates where bytes will fall</param>
/// <returns>The (x,y) coordinates of the first byte that blocks all paths</returns>
let findBlockingByte ((gridSize: int, bytePositions): int * (int * int) seq) =
    let positionsList = List.ofSeq bytePositions
    
    // Cache of previous results to avoid redundant calculations
    let solutionCache = Dictionary<int, Option<int>>()
    let lockObj = obj()
    
    // Check if the path exists with the first 'index' bytes
    let isPathSolvable index =
        lock lockObj (fun () ->
            if not (solutionCache.ContainsKey(index)) then
                solutionCache[index] <- findShortestPath gridSize positionsList[..index]
            solutionCache[index]
        )
    
    // Binary search implementation with parallel optimization
    let findBlockingByteIndex() =
        // First, find the approximate range using quick checks
        let mutable lowerBound = 0
        let mutable upperBound = positionsList.Length - 1
        
        // Initial narrowing to get a smaller range
        while upperBound - lowerBound > 50 do
            let middleIndex = (lowerBound + upperBound) / 2
            if isPathSolvable middleIndex |> Option.isSome then
                lowerBound <- middleIndex
            else
                upperBound <- middleIndex
        
        // Once there is a narrower range, check potential blocking positions in parallel
        let candidateIndices = [|lowerBound..upperBound|]
        let results = Array.create candidateIndices.Length None
        
        // Create parallel tasks to check each candidate
        let tasks = 
            candidateIndices
            |> Array.mapi (fun arrayIndex positionIndex -> 
                Task.Run(fun () -> 
                    let result = isPathSolvable positionIndex
                    results[arrayIndex] <- Some (positionIndex, result)
                )
            )
        
        // Wait for all tasks to complete
        Task.WaitAll(tasks)
        
        // Find the first blocking position
        results 
        |> Array.choose id
        |> Array.sortBy fst
        |> Array.tryFind (fun (_, result) -> Option.isNone result)
        |> Option.map fst
        |> Option.defaultValue upperBound
    
    // Get the first blocking byte
    let blockingIndex = findBlockingByteIndex()
    positionsList[blockingIndex]

/// <summary>
/// Parses the input string into a sequence of byte positions
/// </summary>
///
/// <remarks>
/// Expected format:
/// - Each line contains "x,y" coordinates
/// - Coordinates are comma-separated integers
/// </remarks>
///
/// <param name="input">String containing byte position data</param>
/// <returns>Sequence of (x,y) coordinates</returns>
let parseBytePositions (input: string) =
    input.Split"\n" 
    |> Seq.map (fun line ->
        let parts = line.Split","
        int parts[0], int parts[1])

/// <summary>
/// Measures execution time of a function and returns its result
/// </summary>
///
/// <param name="name">Name to display in the timing output</param>
/// <param name="targetFunction">Function to measure</param>
/// <param name="functionArgument">Argument to pass to the function</param>
/// <returns>The result of the function call</returns>
let benchmark name targetFunction functionArgument =
    let stopWatch = Stopwatch.StartNew()
    let result = targetFunction functionArgument
    stopWatch.Stop()
    printfn $"%s{name}: %.4f{stopWatch.Elapsed.TotalSeconds} seconds"
    result

/// <summary>
/// Unit tests and visualization tools for the solution
/// </summary>
module Tests =
    /// <summary>Example input from the challenge</summary>
    let exampleInput = "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0"

    [<Test>]
    let ``Memory grid visualization`` () =
        let corruptedPositions = [(1, 1); (2, 2); (0, 3)]
        let gridSize = 4
        
        /// <summary>Creates and displays a visual representation of the memory grid</summary>
        let visualizeMemoryGrid size positions =
            let grid = Array2D.create (size+1) (size+1) false
            positions |> Seq.iter (fun (x, y) -> 
                if x >= 0 && x <= size && y >= 0 && y <= size then
                    grid[y, x] <- true)
            
            printfn "Memory grid representation (# = corrupted, . = safe):"
            for row in 0..size do
                for col in 0..size do
                    printf $"%c{if grid[row, col] then '#' else '.'}"
                printfn ""
            grid
        
        visualizeMemoryGrid gridSize corruptedPositions |> ignore

    /// <summary>
    /// Demonstrates the shortest path algorithm's behavior with visual tracing.
    /// This test executes the pathfinding algorithm on a small subset of positions
    /// from the example input and provides detailed logging of its operation including
    /// - The first 5 bytes that are considered
    /// - Their exact positions printed in a readable format
    /// - The result of the pathfinding operation with the path length if found
    /// This serves as both a validation test and a debugging/visualization tool.
    /// </summary>
    [<Test>]
    let ``Algorithm trace with step-by-step logging`` () =
        let bytePositions = parseBytePositions exampleInput |> Seq.take 5 |> List.ofSeq
        
        printfn "Tracing first few bytes falling:"
        printfn "Considering positions: %s" 
            (bytePositions |> Seq.map (fun (x, y) -> $"(%d{x},%d{y})") |> String.concat " ")
        
        let result = findShortestPath 6 bytePositions
        match result with
        | Some distance -> printfn $"Shortest path length: %d{distance}"
        | None -> printfn "No path exists"

    /// <summary>Integration test with the example input from the challenge</summary>
    [<Test>]
    let ``Full challenge with example input`` () =
        let bytePositions = parseBytePositions exampleInput
        
        let part1Answer = calculateMinSteps (6, 12, bytePositions)
        printfn $"Part 1: %d{part1Answer}"
        part1Answer |> should equal 22
        
        let part2Answer = findBlockingByte (6, bytePositions)
        printfn $"Part 2: (%d{fst part2Answer},%d{snd part2Answer})"
        part2Answer |> should equal (6, 1)
    
    /// <summary>Performance benchmark of the solution</summary>
    [<Test>]
    let ``Performance benchmarking`` () =
        let bytePositions = parseBytePositions exampleInput
        
        printfn "Running performance benchmark..."
        
        let part1Input = (6, 12, bytePositions)
        let part2Input = (6, bytePositions)
        
        let part1Result = benchmark "Part 1" calculateMinSteps part1Input
        let part2Result = benchmark "Part 2" findBlockingByte part2Input
        
        printfn $"Part 1 result: %d{part1Result}"
        printfn $"Part 2 result: (%d{fst part2Result},%d{snd part2Result})"

/// <summary>Main entry point for the program</summary>
[<EntryPoint>]
let main _ = 
    let input = stdin.ReadToEnd().TrimEnd()
    let bytePositions = parseBytePositions input

    let stopwatch = Stopwatch.StartNew()
    let part1Result = benchmark "Part 1" calculateMinSteps (70, 1024, bytePositions)
    let part2Result = benchmark "Part 2" findBlockingByte (70, bytePositions)
    
    printfn $"Part 1: %d{part1Result}"
    printfn $"Part 2: (%d{fst part2Result},%d{snd part2Result})"
    
    stopwatch.Stop()
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"
    0