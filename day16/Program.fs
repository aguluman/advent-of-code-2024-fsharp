module day16

open System.Diagnostics
open NUnit.Framework
open FsUnit
open System.Collections.Generic

// Simple priority queue implementation
type PriorityQueue<'T>() = 
    let heap = ResizeArray<'T * int>()
    
    member _.Enqueue (item: 'T, priority: int) =
        heap.Add((item, priority))
        let rec bubbleUp i =
            if i = 0 then () else
            let parent = (i - 1) / 2
            if snd heap.[parent] <= snd heap.[i] then ()
            else
                let tmp = heap.[i]
                heap.[i] <- heap.[parent]
                heap.[parent] <- tmp
                bubbleUp parent
        bubbleUp (heap.Count - 1)
    
    member _.Dequeue() =
        if heap.Count = 0 then failwith "Empty queue"
        let result = fst heap.[0]
        
        heap.[0] <- heap.[heap.Count - 1]
        heap.RemoveAt(heap.Count - 1)
        
        let rec bubbleDown i =
            let left = 2 * i + 1
            let right = 2 * i + 2
            let smallest = 
                if left >= heap.Count then i
                elif snd heap.[left] < snd heap.[i] then left
                else i
            let smallest =
                if right >= heap.Count then smallest
                elif snd heap.[right] < snd heap.[smallest] then right
                else smallest
            if smallest = i then ()
            else
                let tmp = heap.[i]
                heap.[i] <- heap.[smallest]
                heap.[smallest] <- tmp
                bubbleDown smallest
        
        if heap.Count > 0 then bubbleDown 0
        result
    
    member _.IsEmpty = heap.Count = 0
    member _.Count = heap.Count

type Direction =
    | E
    | N
    | W
    | S

    member this.CCW() = 
        match this with 
        | E -> N
        | N -> W
        | W -> S
        | S -> E

    member this.CW() = 
        match this with
        | E -> S
        | S -> W
        | W -> N
        | N -> E
        


type Node = {
    Row: int
    Column: int
    Direction: Direction
} with

    member this.Forward(maze: char[][]) =
        let nRow, nColumn = 
            match this.Direction with 
            | E -> this.Row, this.Column + 1
            | N -> this.Row - 1, this.Column
            | W -> this.Row, this.Column - 1
            | S -> this.Row + 1, this.Column

        if 
            nRow >= 0
            && nRow < maze.Length
            && nColumn >= 0
            && nColumn < maze[nRow].Length
            && maze[nRow][nColumn] <> '#'
        then 
            Some { this with Row = nRow; Column = nColumn }
        else
            None

    member this.Rotate() =
        [ { this with Direction = this.Direction.CCW() };
          { this with Direction = this.Direction.CW() } ]


type Edge = {
    From: Node;
    To: Node;
    Cost: int
}


let dijkstra (start: Node) (edges: Edge list) = 
    let adjacent = edges |> List.groupBy (fun e -> e.From) |> Map.ofList
    
    let pq = PriorityQueue<Node>()
    let distances = Dictionary<Node, int>()
    
    pq.Enqueue(start, 0)
    distances.[start] <- 0
    
    let mutable result = Map.empty
    
    while not pq.IsEmpty do
        let node = pq.Dequeue()
        let dist = distances.[node]
        
        if not (result.ContainsKey node) then
            result <- result.Add(node, dist)
            
            match Map.tryFind node adjacent with
            | Some neighbors ->
                for edge in neighbors do
                    let newDist = dist + edge.Cost
                    
                    let shouldUpdate =
                        match distances.TryGetValue(edge.To) with
                        | true, oldDist -> newDist < oldDist
                        | false, _ -> true
                    
                    if shouldUpdate then
                        distances.[edge.To] <- newDist
                        pq.Enqueue(edge.To, newDist)
            | None -> ()
    
    result


let collectEdges (maze: char[][]) =
    List.allPairs [ 0 .. (maze.Length - 1) ] [ 0 .. (maze[0].Length - 1) ] 
    |> List.collect (fun (i, j) -> 
        if maze[i][j] = '#' then
            []
        else
            [ E; N; W; S; ]
            |> List.collect (fun direction -> 
            let node = { Row = i; Column = j; Direction = direction }
            
            let forward =
                node.Forward maze 
                |> Option.map (fun node' -> { From = node; To = node'; Cost = 1 }) 
            
            let rotate =
                node.Rotate() 
                |> List.map (fun node' -> { From = node; To = node'; Cost = 1000 })
                
            match forward with
            | Some forward -> forward :: rotate
            | None -> rotate))


let findIndex2D (a: 'T[][]) (v: 'T) = 
    List.allPairs [ 0 .. (a.Length - 1) ] [ 0 .. (a[0].Length - 1) ]
    |> List.find (fun (i, j) -> a[i][j] = v)


let getNeighbors (node: Node) (maze: char[][]) =
    let neighbors = ResizeArray<Node * int>()
    
    // Forward movement
    match node.Forward(maze) with
    | Some nextNode -> neighbors.Add(nextNode, 1)
    | None -> ()
    
    // Rotations
    for rotatedNode in node.Rotate() do
        neighbors.Add(rotatedNode, 1000)
    
    neighbors |> Seq.toList

let dijkstraOnDemand (start: Node) (maze: char[][]) =
    let pq = PriorityQueue<Node>()
    let distances = Dictionary<Node, int>()
    
    pq.Enqueue(start, 0)
    distances.[start] <- 0
    
    let mutable result = Map.empty
    
    while not pq.IsEmpty do
        let node = pq.Dequeue()
        let dist = distances.[node]
        
        if not (result.ContainsKey node) then
            result <- result.Add(node, dist)
            
            for (nextNode, cost) in getNeighbors node maze do
                let newDist = dist + cost
                
                let shouldUpdate =
                    match distances.TryGetValue(nextNode) with
                    | true, oldDist -> newDist < oldDist
                    | false, _ -> true
                
                if shouldUpdate then
                    distances.[nextNode] <- newDist
                    pq.Enqueue(nextNode, newDist)
    
    result

// Replace all uses of dijkstra with dijkstraOnDemand
let part1 (maze: char[][]) = 
    let start_i, start_j = findIndex2D maze 'S'
    let end_i, end_j = findIndex2D maze 'E'

    let start = {
        Row = start_i
        Column = start_j
        Direction = E
    }

    let distance = dijkstraOnDemand start maze

    [E; N; W; S]
    |> List.map (fun direction -> 
        let goal = {
            Row = end_i;
            Column = end_j;
            Direction = direction
        }
        match Map.tryFind goal distance with
        | Some d -> d
        | None -> System.Int32.MaxValue)
    |> List.min


let part2 (maze: char[][]) =
    let start_i, start_j = findIndex2D maze 'S'
    let end_i, end_j = findIndex2D maze 'E'

    let start = {
        Row = start_i
        Column = start_j
        Direction = E
    }

    // Run forward Dijkstra once
    let forwardDist = dijkstraOnDemand start maze

    // Find the best direction at the endpoint
    let bestDir = 
        [E; N; W; S]
        |> List.minBy (fun direction -> 
            let goal = {
                Row = end_i;
                Column = end_j;
                Direction = direction
            }
            match Map.tryFind goal forwardDist with
            | Some d -> d
            | None -> System.Int32.MaxValue)
    
    let endNode = { 
        Row = end_i
        Column = end_j
        Direction = bestDir }
    
    let minDist = 
        match Map.tryFind endNode forwardDist with
        | Some d -> d
        | None -> System.Int32.MaxValue
    
    // Create a reversed maze for backward Dijkstra
    let revMaze = Array.map Array.copy maze
    
    // Run backward Dijkstra
    let backwardDist = dijkstraOnDemand endNode maze
    
    // Use HashSet for faster lookup
    let onPath = HashSet<int * int>()
    
    for i in 0 .. maze.Length - 1 do
        for j in 0 .. maze[0].Length - 1 do
            if maze[i][j] <> '#' then
                for dir in [E; N; W; S] do
                    let node = { Row = i; Column = j; Direction = dir }
                    match Map.tryFind node forwardDist, Map.tryFind node backwardDist with
                    | Some fd, Some bd when fd + bd = minDist -> 
                        onPath.Add((i, j)) |> ignore
                    | _ -> ()
    
    onPath.Count
    


let parse (input: string) =
    input.Split([| '\r'; '\n'|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun row -> row.Trim().ToCharArray())

        
module Tests =
    // Example maze inputs from the challenge
    let exampleMaze1 = "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############"

    let exampleMaze2 = "#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################"

    // Helper function for maze visualization
    let printMaze (maze: char[][]) =
        printfn "\nMaze representation:"
        maze |> Array.iter (fun row ->
            row |> Array.iter (fun cell -> printf "%c" cell)
            printfn "")

    [<TestFixture>]
    type Day16Tests() =
        
        [<Test>]
        member _.``lowest point a Reindeer could possibly get``() =
            let maze = parse exampleMaze1
            part1 maze |> should equal 7036

        [<Test>]
        member _.``lowest point a Reindeer could possibly get on a larger hill``() =
            let maze = parse exampleMaze2
            part1 maze |> should equal 11048
            
        [<Test>]
        member _.``points on optimal path through Reindeer Hills``() =
            let maze = parse exampleMaze1
            part2 maze |> should equal 45
            
        [<Test>]
        member _.``points on optimal path through larger Reindeer hills``() =
            let maze = parse exampleMaze2
            part2 maze |> should equal 64
            
        [<Test>]
        member _.``maze visualization``() =
            let maze = parse exampleMaze1
            printMaze maze


[<EntryPoint>]
let main _ = 
    let input = stdin.ReadToEnd().TrimEnd()
    let maze = parse input

    let stopwatch = Stopwatch.StartNew()

    maze |> part1 |> printfn "Part 1: %d"
    maze |> part2 |> printfn "Part 2: %d"

    stopwatch.Stop()
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"

    0