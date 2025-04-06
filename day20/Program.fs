/// <summary>
/// Day 20: Maze Optimization - Path Shortening Challenge
/// </summary>
/// <description>
/// Solves Advent of Code Day 20 challenge about finding optimal paths through a maze.
/// The module analyzes potential shortcuts by removing walls and calculates distance improvements.
/// </description>
///
/// <remarks>
/// Problem details:
/// - Input: A 2D maze with '#' as walls, 'S' as start, 'E' as the end, and '.' as open spaces
/// - Part 1: Identify which wall removals create shortcuts and calculate their impact
/// - Part 2: Analyze path inefficiencies by comparing actual vs. Manhattan distances
///
/// The solutions use breadth-first search with parallelism for performance optimization.
///
/// See: <see href="https://adventofcode.com/2024/day/20">Advent of Code 2024, Day 20</see>
/// </remarks>
module day20

open System.Diagnostics
open System.Collections.Generic
open System.Threading.Tasks
open NUnit.Framework
open FsUnit


/// <summary>
/// Finds the coordinates of a specific value in a 2D array.
/// </summary>
///
/// <remarks>
/// Uses list comprehension to generate all possible coordinates
/// and then finds the first occurrence of the target value.
/// </remarks>
///
/// <param name="grid">The 2D array to search</param>
/// <param name="target">The value to find</param>
/// <returns>A tuple (row, col) representing the row and column indices of the value</returns>
let findIndex2D (grid: 'T[][]) (target: 'T) =
    List.allPairs [ 0 .. (grid.Length - 1) ] [ 0 .. (grid[0].Length - 1) ]
    |> List.find (fun (row, col) -> grid[row][col] = target)


/// <summary>
/// Performs an optimized breadth-first search on a 2D maze to find the shortest paths.
/// </summary>
///
/// <remarks>
/// Implements BFS with several performance optimizations:
/// - Uses struct tuples to reduce memory allocations
/// - Pre-calculates maze bounds to avoid repeated bounds checks
/// - Manually unrolls direction loops for better performance
/// - Uses a dictionary to track visited cells and their distances
/// </remarks>
///
/// <param name="startRow">Starting row index</param>
/// <param name="startCol">Starting column index</param>
/// <param name="maze">2D character array representing the maze</param>
/// <returns>Dictionary mapping cell coordinates to their shortest distance from start</returns>
let bfsFast (startRow, startCol) (maze: char[][]) =
    let queue = Queue<struct (int * int)>(maze.Length * maze[0].Length)
    queue.Enqueue(struct (startRow, startCol))

    let distances = Dictionary<struct (int * int), int>(maze.Length * maze[0].Length)
    distances[struct (startRow, startCol)] <- 0

    // Pre-calculate bounds
    let maxRow, maxCol = maze.Length - 1, maze[0].Length - 1

    while queue.Count > 0 do
        let struct (row, col) = queue.Dequeue()
        let distance = distances[struct (row, col)]
        let nextDistance = distance + 1

        // Manually unrolled loop with bounds checking first
        if row > 0 && maze[row - 1].[col] <> '#' then
            let nextPos = struct (row - 1, col)

            if not (distances.ContainsKey(nextPos)) then
                distances[nextPos] <- nextDistance
                queue.Enqueue(nextPos)

        if col > 0 && maze[row].[col - 1] <> '#' then
            let nextPos = struct (row, col - 1)

            if not (distances.ContainsKey(nextPos)) then
                distances[nextPos] <- nextDistance
                queue.Enqueue(nextPos)

        if row < maxRow && maze[row + 1].[col] <> '#' then
            let nextPos = struct (row + 1, col)

            if not (distances.ContainsKey(nextPos)) then
                distances[nextPos] <- nextDistance
                queue.Enqueue(nextPos)

        if col < maxCol && maze[row].[col + 1] <> '#' then
            let nextPos = struct (row, col + 1)

            if not (distances.ContainsKey(nextPos)) then
                distances[nextPos] <- nextDistance
                queue.Enqueue(nextPos)

    distances



/// <summary>
/// Identifies walls that, when removed, create shortcuts and measures their impact.
/// </summary>
///
/// <remarks>
/// Algorithm steps:
/// 1. Find start (S) and end (E) points in maze
/// 2. Compute the shortest path from S to E in the original maze
/// 3. Identify walls adjacent to the path using BFS
/// 4. For each wall, remove it and recalculate paths in parallel
/// 5. Calculate improvement in distance when each wall is removed
///
/// The function returns a list of (improvement, count) tuples, where:
/// - improvement: how many steps are saved by removing a wall
/// - count: how many different walls provide this same improvement
/// </remarks>
///
/// <param name="maze">2D character array representing the maze</param>
/// <returns>List of (improvement, count) tuples sorted by improvement value</returns>
let part1 (maze: char[][]) =
    let startRow, startCol = findIndex2D maze 'S'
    let endRow, endCol = findIndex2D maze 'E'

    let distancesFromStart = bfsFast (startRow, startCol) maze
    let endPosition = struct (endRow, endCol)

    if not (distancesFromStart.ContainsKey(endPosition)) then
        []
    else
        let originalDistance = distancesFromStart[endPosition]
        let visited = HashSet<struct (int * int)>()
        let walls = HashSet<struct (int * int)>()

        // Find walls along the path using BFS
        let queue = Queue<struct (int * int)>()
        queue.Enqueue(endPosition)
        visited.Add(endPosition) |> ignore

        while queue.Count > 0 do
            let struct (row, col) = queue.Dequeue()

            for deltaRow, deltaCol in [| (-1, 0); (1, 0); (0, -1); (0, 1) |] do
                let neighborRow, neighborCol = row + deltaRow, col + deltaCol

                if
                    neighborRow >= 0
                    && neighborRow < maze.Length
                    && neighborCol >= 0
                    && neighborCol < maze[0].Length
                then
                    let neighborPos = struct (neighborRow, neighborCol)

                    if maze[neighborRow].[neighborCol] = '#' then
                        walls.Add(neighborPos) |> ignore
                    elif
                        not (visited.Contains(neighborPos))
                        && distancesFromStart.ContainsKey(neighborPos)
                    then
                        visited.Add(neighborPos) |> ignore
                        queue.Enqueue(neighborPos)

        // Process walls in parallel with minimal copying
        let improvements = Dictionary<int, int>()
        let lockObj = obj ()

        // Fixed parallel processing block
        Parallel.ForEach(
            walls,
            fun wallPos ->
                let struct (wallRow, wallCol) = wallPos
                let mazeWithRemovedWall = Array.map Array.copy maze
                mazeWithRemovedWall[wallRow].[wallCol] <- '.'

                let newDistances = bfsFast (startRow, startCol) mazeWithRemovedWall

                if newDistances.ContainsKey(endPosition) then
                    let improvement = originalDistance - newDistances[endPosition]

                    if improvement > 0 then
                        lock lockObj (fun () ->
                            improvements[improvement] <-
                                if improvements.ContainsKey(improvement) then
                                    improvements[improvement] + 1
                                else
                                    1)
        )
        |> ignore

        improvements
        |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
        |> Seq.toList
        |> List.sort




/// <summary>
/// Analyzes path inefficiency by comparing actual path distances with Manhattan distances.
/// </summary>
///
/// <remarks>
/// Algorithm steps:
/// 1. Find start point (S) in the maze
/// 2. Compute the shortest paths from S to all reachable points
/// 3. For each pair of points:
///    - Calculate Manhattan distance between them
///    - Compare with actual path distances
///    - Record positive differences (inefficiencies)
/// 4. Process point pairs in parallel for better performance
///
/// The function returns a list of (inefficiency, count) tuples, where:
/// - inefficiency: extra steps in the path compared to Manhattan distance
/// - count: how many point pairs have this same inefficiency value
/// </remarks>
///
/// <param name="maze">2D character array representing the maze</param>
/// <returns>List of (inefficiency, count) tuples sorted by inefficiency value</returns>
let part2 (maze: char[][]) =
    let startRow, startCol = findIndex2D maze 'S'
    let distances = bfsFast (startRow, startCol) maze

    let inefficiencies = Dictionary<int, int>()
    let reachablePoints = distances.Keys |> Seq.toArray

    // Process in chunks for better CPU utilization
    Parallel.For(
        0,
        reachablePoints.Length,
        fun i ->
            let struct (row1, col1) = reachablePoints[i]
            let distance1 = distances[struct (row1, col1)]

            let localInefficiencies = Dictionary<int, int>()

            for j = i + 1 to reachablePoints.Length - 1 do
                let struct (row2, col2) = reachablePoints[j]
                let distance2 = distances[struct (row2, col2)]

                let manhattanDistance = abs (row1 - row2) + abs (col1 - col2)

                if manhattanDistance <= 20 then
                    let inefficiency1 = distance2 - distance1 - manhattanDistance

                    if inefficiency1 >= 0 then
                        localInefficiencies[inefficiency1] <- localInefficiencies.GetValueOrDefault(inefficiency1) + 1

                    let inefficiency2 = distance1 - distance2 - manhattanDistance

                    if inefficiency2 >= 0 then
                        localInefficiencies[inefficiency2] <- localInefficiencies.GetValueOrDefault(inefficiency2) + 1

            lock inefficiencies (fun () ->
                for KeyValue(inefficiency, count) in localInefficiencies do
                    inefficiencies[inefficiency] <- inefficiencies.GetValueOrDefault(inefficiency) + count)
    )
    |> ignore

    inefficiencies
    |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
    |> Seq.toList
    |> List.sort




/// <summary>
/// Parses the input string into a 2D character array representing the maze.
/// </summary>
///
/// <remarks>
/// Splits the input by newlines and converts each line to a character array,
/// ensuring trailing whitespace is removed for consistent maze dimensions.
/// </remarks>
///
/// <param name="input">Raw input string</param>
/// <returns>2D character array representing the maze</returns>
let parse (input: string) =
    input.Split("\n") |> Array.map _.TrimEnd().ToCharArray()



/// <summary>
/// Example test cases and assertions for the maze optimization solution
/// </summary>
module Example =
    /// <summary>Example maze from the challenge description</summary>
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

    /// <summary>Tests that Part 1 correctly identifies wall removals and their improvements</summary>
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

    /// <summary>Tests that Part 2 correctly analyzes path inefficiencies</summary>
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



/// <summary>Main entry point for the program</summary>
[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let maze = parse input

    let stopwatch = Stopwatch()
    stopwatch.Start()

    maze
    |> part1
    |> Seq.sumBy (fun (improvement, count) -> if improvement >= 100 then count else 0)
    |> printfn "Part 1: %d"

    stopwatch.Stop()
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"


    stopwatch.Restart()

    maze
    |> part2
    |> Seq.sumBy (fun (improvement, count) -> if improvement >= 100 then count else 0)
    |> printfn "Part 2: %d"

    stopwatch.Stop()
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"

    0
