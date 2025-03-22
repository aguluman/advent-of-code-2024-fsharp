module day18

open System.Diagnostics
open System.Collections.Generic
open NUnit.Framework
open FsUnit

let solve n positions = 
    // Convert to array for faster lookup during BFS
    let positionsSet = Set.ofSeq positions
    
    // Use mutable data structures for BFS
    let queue = Queue<int * int>()
    let distance = Dictionary<int * int, int>()
    
    // Initialize with starting position
    queue.Enqueue((0, 0))
    distance.[(0, 0)] <- 0
    
    // Early termination flag
    let mutable found = false
    
    // BFS implementation with early termination
    while not found && queue.Count > 0 do
        let (x, y) = queue.Dequeue()
        let currentDist = distance.[(x, y)]
        
        // Check if we've reached the target
        if (x, y) = (n, n) then
            found <- true
        else
            // Check all four directions
            for (dx, dy) in [(-1, 0); (0, -1); (1, 0); (0, 1)] do
                let nx, ny = x + dx, y + dy
                
                // Check bounds and if position is valid
                if nx >= 0 && nx <= n && 
                   ny >= 0 && ny <= n && 
                   not (Set.contains (nx, ny) positionsSet) && 
                   not (distance.ContainsKey(nx, ny)) then
                    
                    queue.Enqueue((nx, ny))
                    distance.[(nx, ny)] <- currentDist + 1
    
    // Return the shortest path length if found
    if distance.ContainsKey((n, n)) then 
        Some distance.[(n, n)] 
    else 
        None

let part1 ((n, bytes, positions): int * int * (int * int) seq) =
    solve n (Seq.take bytes positions) |> Option.get

let part2 ((n: int, positions): (int * (int * int) seq)) =
    let positions = List.ofSeq positions
    
    // Cache of previous solve results to avoid redundant calculations
    let solutionCache = Dictionary<int, Option<int>>()
    
    let checkSolvable mid =
        if not (solutionCache.ContainsKey(mid)) then
            solutionCache.[mid] <- solve n positions[..mid]
        solutionCache.[mid]
    
    // Binary search to find the first blocking position
    let rec bisect ok ng =
        if ok + 1 = ng then 
            ng
        else 
            let mid = (ok + ng) / 2
            
            if checkSolvable mid |> Option.isSome then
                bisect mid ng
            else
                bisect ok mid
    
    let i = bisect 0 positions.Length
    positions[i]

let parse (input: string) =
    input.Split"\n" 
    |> Seq.map (fun line ->
        let line = line.Split","
        int line[0], int line[1])

module Tests =
    // Example input from the challenge
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
    let ``grid visualization`` () =
        let positions = [(1, 1); (2, 2); (0, 3)]
        let n = 4
        
        // Function to create and display the grid
        let createAndShowGrid n positions =
            let grid = Array2D.create (n+1) (n+1) false
            positions |> Seq.iter (fun (x, y) -> 
                if x >= 0 && x <= n && y >= 0 && y <= n then
                    grid[y, x] <- true)
            
            printfn "Memory grid representation (# = corrupted, . = safe):"
            for y in 0..n do
                for x in 0..n do
                    printf "%c" (if grid[y, x] then '#' else '.')
                printfn ""
            grid
        
        createAndShowGrid n positions |> ignore

    [<Test>]
    let ``algorithm tracing`` () =
        let positions = parse exampleInput |> Seq.take 5 |> List.ofSeq
        
        printfn "Tracing first few bytes falling:"
        printfn "Considering positions: %s" 
            (positions |> Seq.map (fun (x, y) -> sprintf "(%d,%d)" x y) |> String.concat " ")
        
        let result = solve 6 positions
        match result with
        | Some dist -> printfn "Shortest path length: %d" dist
        | None -> printfn "No path exists"


    // Integration tests that mimic the main function execution
    [<Test>]
    let ``full challenge with example input`` () =
        let positions = parse exampleInput
        
        let part1Answer = part1 (6, 12, positions)
        printfn "Part 1: %d" part1Answer
        part1Answer |> should equal 22
        
        let part2Answer = part2 (6, positions)
        printfn "Part 2: (%d,%d)" (fst part2Answer) (snd part2Answer)
        part2Answer |> should equal (6, 1)



[<EntryPoint>]
let main _ = 
    let input = stdin.ReadToEnd().TrimEnd()
    let positions = parse input

    let stopwatch = Stopwatch.StartNew()
    (70, 1024, positions) |> part1 |> printfn "Part 1: %d"
    (70, positions) |> part2 |> printfn "Part 2: %A"

    stopwatch.Stop()
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"
    0