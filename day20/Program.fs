module day20

open System.Diagnostics
open System.Collections.Generic
open System.Threading.Tasks
open NUnit.Framework
open FsUnit


let findIndex2D (a: 'T[][]) (v: 'T) =
    List.allPairs [ 0 .. (a.Length - 1) ] [ 0 .. (a.[0].Length - 1) ]
    |> List.find (fun (i, j) -> a[i][j] = v)



let bfsFast (si, sj) (maze: char[][]) = 
    let queue = Queue<struct(int * int)>(maze.Length * maze.[0].Length)
    queue.Enqueue(struct(si, sj))
    
    let dist = Dictionary<struct(int * int), int>(maze.Length * maze.[0].Length)
    dist.[struct(si, sj)] <- 0
    
    // Pre-calculate bounds
    let rowBound, colBound = maze.Length - 1, maze.[0].Length - 1
    
    while queue.Count > 0 do
        let struct(i, j) = queue.Dequeue()
        let d = dist.[struct(i, j)]
        let dNext = d + 1
        
        // Manually unrolled loop with bounds checking first
        if i > 0 && maze.[i-1].[j] <> '#' then
            let next = struct(i-1, j)
            if not (dist.ContainsKey(next)) then
                dist.[next] <- dNext
                queue.Enqueue(next)
                
        if j > 0 && maze.[i].[j-1] <> '#' then
            let next = struct(i, j-1)
            if not (dist.ContainsKey(next)) then
                dist.[next] <- dNext
                queue.Enqueue(next)
                
        if i < rowBound && maze.[i+1].[j] <> '#' then
            let next = struct(i+1, j)
            if not (dist.ContainsKey(next)) then
                dist.[next] <- dNext
                queue.Enqueue(next)
                
        if j < colBound && maze.[i].[j+1] <> '#' then
            let next = struct(i, j+1)
            if not (dist.ContainsKey(next)) then
                dist.[next] <- dNext
                queue.Enqueue(next)
    dist

let part1 (maze: char[][]) =
    let si, sj = findIndex2D maze 'S'
    let ei, ej = findIndex2D maze 'E'
    
    let distFromS = bfsFast (si, sj) maze
    let target = struct(ei, ej)
    
    if not (distFromS.ContainsKey(target)) then []
    else
        let currentBest = distFromS.[target]
        let visited = HashSet<struct(int * int)>()
        let walls = HashSet<struct(int * int)>()
        
        // Find walls along the path using BFS
        let queue = Queue<struct(int * int)>()
        queue.Enqueue(target)
        visited.Add(target) |> ignore
        
        while queue.Count > 0 do
            let struct(i, j) = queue.Dequeue()
            for di, dj in [|(-1,0); (1,0); (0,-1); (0,1)|] do
                let ni, nj = i + di, j + dj
                if ni >= 0 && ni < maze.Length && nj >= 0 && nj < maze.[0].Length then
                    let next = struct(ni, nj)
                    if maze.[ni].[nj] = '#' then
                        walls.Add(next) |> ignore
                    elif not (visited.Contains(next)) && distFromS.ContainsKey(next) then
                        visited.Add(next) |> ignore
                        queue.Enqueue(next)
        
        // Process walls in parallel with minimal copying
        let results = Dictionary<int, int>()
        let lockObj = obj()
        
        // Fixed parallel processing block
        Parallel.ForEach(walls, fun wall ->
            let struct(wi, wj) = wall
            let mazeCopy = Array.map Array.copy maze
            mazeCopy.[wi].[wj] <- '.'
            
            let newDist = bfsFast (si, sj) mazeCopy
            if newDist.ContainsKey(target) then
                let improvement = currentBest - newDist.[target]
                if improvement > 0 then
                    lock lockObj (fun () ->
                        results.[improvement] <- 
                            if results.ContainsKey(improvement) then 
                                results.[improvement] + 1 
                            else 1)
        ) |> ignore
        
        results |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Seq.toList |> List.sort

let part2 (maze: char[][]) = 
    let si, sj = findIndex2D maze 'S'
    let dist = bfsFast (si, sj) maze
    
    let results = Dictionary<int, int>()
    let points = dist.Keys |> Seq.toArray
    
    // Process in chunks for better CPU utilization
    Parallel.For(0, points.Length, fun i ->
        let struct(x1, y1) = points.[i]
        let d1 = dist.[struct(x1, y1)]
        
        let localResults = Dictionary<int, int>()
        for j = i + 1 to points.Length - 1 do
            let struct(x2, y2) = points.[j]
            let d2 = dist.[struct(x2, y2)]
            
            let e = abs(x1 - x2) + abs(y1 - y2)
            if e <= 20 then
                let diff1 = d2 - d1 - e
                if diff1 >= 0 then
                    localResults.[diff1] <- localResults.GetValueOrDefault(diff1) + 1
                
                let diff2 = d1 - d2 - e
                if diff2 >= 0 then
                    localResults.[diff2] <- localResults.GetValueOrDefault(diff2) + 1
                    
        lock results (fun () ->
            for KeyValue(k, v) in localResults do
                results.[k] <- results.GetValueOrDefault(k) + v)
    ) |> ignore
    
    results |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Seq.toList |> List.sort

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

    let timer = Stopwatch()
    timer.Start()

    maze
    |> part1
    |> Seq.sumBy (fun (save, ps) -> if save >= 100 then ps else 0)
    |> printfn "Part 1: %d"

    timer.Stop()
    printfn $"Elapsed time: %.4f{timer.Elapsed.TotalSeconds} seconds"


    timer.Restart()

    maze
    |> part2
    |> Seq.sumBy (fun (save, ps) -> if save >= 100 then ps else 0)
    |> printfn "Part 2: %d"

    timer.Stop()
    printfn $"Elapsed time: %.4f{timer.Elapsed.TotalSeconds} seconds"

    0
