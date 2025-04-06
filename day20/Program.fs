module day20

open System.Diagnostics
open System.Collections.Generic
open System.Threading.Tasks
open NUnit.Framework
open FsUnit


let findIndex2D (a: 'T[][]) (v: 'T) =
    List.allPairs [ 0 .. (a.Length - 1) ] [ 0 .. (a.[0].Length - 1) ]
    |> List.find (fun (i, j) -> a[i][j] = v)



let bfs (si, sj) (maze: char[][]) = 
    let queue = Queue<int * int>()
    queue.Enqueue((si, sj))
    
    let mutable dist = Map.ofList [((si, sj), 0)]
    
    while queue.Count > 0 do
        let (i, j) = queue.Dequeue()
        let currentDist = dist.[(i, j)]
        
        for (di, dj) in [(-1, 0); (0, -1); (1, 0); (0, 1)] do
            let ni, nj = i + di, j + dj
            if ni >= 0 && ni < maze.Length && 
               nj >= 0 && nj < maze[ni].Length && 
               maze[ni][nj] <> '#' && 
               not (dist.ContainsKey(ni, nj)) then
                dist <- dist.Add((ni, nj), currentDist + 1)
                queue.Enqueue((ni, nj))
                
    dist


// Helper to check if a position is valid and reachable in a distance map
let isReachable (i, j) (maze: char[][]) (dist: Map<int * int, int>) =
    i >= 0 && i < maze.Length && j >= 0 && j < maze[i].Length && dist.ContainsKey(i, j)

// Check if a wall has open paths on opposite sides
let isBreakableWall (i, j) (maze: char[][]) =
    maze[i][j] = '#' && 
    ((i >= 1 && i + 1 < maze.Length && maze[i - 1][j] <> '#' && maze[i + 1][j] <> '#') ||
     (j >= 1 && j + 1 < maze[i].Length && maze[i][j - 1] <> '#' && maze[i][j + 1] <> '#'))

// Check if breaking a wall would connect areas reachable from S and E
let wouldConnectSE (i, j) (maze: char[][]) (distFromS: Map<int * int, int>) (distFromE: Map<int * int, int>) =
    let adjacent = [(i-1, j); (i+1, j); (i, j-1); (i, j+1)]
    let reachableFromS = adjacent |> List.exists (fun pos -> isReachable pos maze distFromS)
    let reachableFromE = adjacent |> List.exists (fun pos -> isReachable pos maze distFromE)
    reachableFromS && reachableFromE




let part1 (maze: char[][]) =
    let si, sj = findIndex2D maze 'S'
    let ei, ej = findIndex2D maze 'E'

    let distFromS = bfs (si, sj) maze
    let distFromE = bfs (ei, ej) maze
    
    if not (distFromS.ContainsKey(ei, ej)) then
        // No path exists, return empty result
        []
    else
        // Find walls that could be broken to improve the path
        let potentialWalls =
            List.allPairs [ 0 .. (maze.Length - 1) ] [ 0 .. (maze[0].Length - 1) ]
            |> List.filter (fun (i, j) -> 
                isBreakableWall (i, j) maze && 
                wouldConnectSE (i, j) maze distFromS distFromE)
                 
        // Create a copy of the original maze to avoid threading issues
        let mazeCopy = maze |> Array.map Array.copy
        
        // Process walls in parallel
        let results =
            potentialWalls
            |> Array.ofList
            |> Array.Parallel.choose (fun (i, j) ->
                // Create a local copy for this thread
                let localMaze = mazeCopy |> Array.map Array.copy
                localMaze[i][j] <- '.'
                
                // Run BFS with removed wall
                let d = bfs (si, sj) localMaze
                
                // Calculate improvement
                if Map.containsKey (ei, ej) d then
                    Some(distFromS[(ei, ej)] - d[(ei, ej)])
                else
                    None)
            |> List.ofArray

        results
        |> List.countBy id
        |> List.sort



let part2 (maze: char[][]) = 
    let si, sj = findIndex2D maze 'S'

    let dist = bfs (si, sj) maze

    List.allPairs (Map.toList dist) (Map.toList dist)
    |> List.choose (fun (((i, j), d), ((i', j'), d')) ->
        let e = abs (i - i') + abs (j - j')
        if d' - d >= 0 && e <= 20 then Some(d' - d - e) else None)
    |> List.countBy id
    |> List.sort


let parse (input: string) =
    input.Split("\n") |> Array.map (fun line -> line.TrimEnd().ToCharArray())


module Example =
    let input =
        "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############"

    [<Test>]
    let testPart1 () =
        parse input
        |> part1
        |> should
            be
            (supersetOf
                [ (2, 14)
                  (4, 14)
                  (6, 2)
                  (8, 4)
                  (10, 2)
                  (12, 3)
                  (20, 1)
                  (36, 1)
                  (38, 1)
                  (40, 1)
                  (64, 1) ])

    [<Test>]
    let testPart2 () =
        parse input
        |> part2
        |> should
            be
            (supersetOf
                [ (50, 32)
                  (52, 31)
                  (54, 29)
                  (56, 39)
                  (58, 25)
                  (60, 23)
                  (62, 20)
                  (64, 19)
                  (66, 12)
                  (68, 14)
                  (70, 12)
                  (72, 22)
                  (74, 4)
                  (76, 3) ])

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let maze = parse input

    let stopwatch = Stopwatch.StartNew()

    maze
    |> part1
    |> Seq.sumBy (fun (save, ps) -> if save >= 100 then ps else 0)
    |> printfn "Part 1: %d"



    maze
    |> part2
    |> Seq.sumBy (fun (save, ps) -> if save >= 100 then ps else 0)
    |> printfn "Part 2: %d"

    stopwatch.Stop()
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"

    0