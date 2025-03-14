module day16

open System.Diagnostics
open NUnit.Framework
open FsUnit

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
    let adjacent = edges |> List.groupBy (fun e -> e.From) |> Map

    let relax (from: Node) (e: Edge) (dist: Map<Node, int>) =
        let distance = dist[from]
        let newDistance = distance + e.Cost

        match Map.tryFind e.To dist with
        | Some distance' when newDistance < distance' -> dist |> Map.add e.To newDistance |> Some
        | None -> dist |> Map.add e.To newDistance |> Some
        | _ -> None

    let transition (node: Node) distance = 
        let newNodes, distance =
            (distance, adjacent[node])
            ||> List.mapFold (fun distance edge -> 
                match relax node edge distance with
                | Some distance -> Some edge.To, distance
                | None -> None, distance)

        List.choose id newNodes, distance
    
    let rec solve (nodes: Node list) distance =
        if List.isEmpty nodes then
            distance
        else
            let newNodes, newDistance =
                (distance, nodes) ||> List.mapFold (
                    fun distance node -> transition node distance)

            solve (List.concat newNodes) newDistance
    
    solve [ start ] (Map [ (start, 0)])


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


let part1 (maze: char[][]) = 
    let edges = collectEdges maze
    let start_i, start_j = findIndex2D maze 'S'
    let end_i, end_j = findIndex2D maze 'E'

    let start = {
        Row = start_i
        Column = start_j
        Direction = E
    }

    let distance = dijkstra start edges

    [E; N; W; S]
    |> List.map (fun direction -> 
        let goal = {
            Row = end_i;
            Column = end_j;
            Direction = direction
        }
        distance[goal])
    |> List.min


let part2 (maze: char[][]) =
    let edges = collectEdges maze
    let start_i, start_j = findIndex2D maze 'S'
    let end_i, end_j = findIndex2D maze 'E'

    let start = {
        Row = start_i
        Column = start_j
        Direction = E
    }

    let distance = dijkstra start edges

    let direction =
        [E; N; W; S]
        |> List.minBy (fun direction -> 
            let goal = {
                Row = end_i;
                Column = end_j;
                Direction = direction
            }
            distance[goal])

    let goal = { 
        Row = end_i
        Column = end_j
        Direction = direction }

    let reversedEdges = edges |> List.map (fun edge -> { edge with From = edge.To; To = edge.From })
    let reversedPathDistance = dijkstra goal reversedEdges

    let minDistance = part1 maze

    List.allPairs [ 0 .. (maze.Length - 1) ] [ 0 .. (maze[0].Length - 1) ]
    |> List.filter (fun (i, j) -> 
        if maze[i][j] = '#' then
            false
        else
            [ E; N; W; S ]
            |> List.exists (fun direction ->
                let node = {
                    Row = i;
                    Column = j;
                    Direction = direction
                }
                match Map.tryFind node distance, Map.tryFind node reversedPathDistance with
                | Some d, Some rd when d + rd = minDistance -> true
                | _ -> false))
    |> List.length
    


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