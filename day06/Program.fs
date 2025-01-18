open Xunit
open FsUnit.Xunit
open System.Collections.Generic

type Direction = 
    | Up
    | Right
    | Down
    | Left

    member this.Next() = 
        match this with 
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up


let part1 (map: char[][]) =
    let n = map.Length

    if Array.exists (fun (row: char[]) -> row.Length <> n) map then
        failwith "Invalid map: not all rows have the same length"

    let rec forward (i, j) dir history =
        match dir with
        | Up -> if i >= 1 then Some(i - 1, j) else None
        | Right -> if j < n - 1 then Some(i, j + 1) else None
        | Down -> if i < n - 1 then Some(i + 1, j) else None
        | Left -> if j >= 1 then Some(i, j - 1) else None
        |> Option.map (fun (ni, nj) ->
            let ni, nj, nd, nh =
                if map[ni][nj] = '#' then
                    (i, j, dir.Next(), history)
                else
                    (ni, nj, dir, (ni, nj) :: history)

            (ni, nj, nd, nh))
        |> Option.defaultWith (fun () -> (i, j, dir, history))

    let rec loop (i, j, dir, history) =
        let ni, nj, nd, nh = forward (i, j) dir history

        if (ni, nj) = (i, j) && dir = nd then
            history |> List.distinct |> List.length
        else
            loop (ni, nj, nd, nh)

    let si, sj =
        List.allPairs [ 0 .. (n - 1) ] [ 0 .. (n - 1) ]
        |> List.find (fun (i, j) -> map[i][j] = '^')

    loop (si, sj, Up, [ (si, sj) ])
    

let part2 (map: char[][]) =
    let n = map.Length
    map |> Array.iter (fun row -> row.Length |> should equal n)

    let findLoop (map: char[][]) (i, j) dir =
        let history = HashSet<int * int * Direction>()
        let stack = Queue<int * int * Direction>()
        stack.Enqueue((i, j, dir))
        let mutable foundLoop = false

        while not foundLoop && stack.Count > 0 do
            let i, j, dir = stack.Dequeue()

            let nextPos =
                match dir with
                | Up -> if i >= 1 then Some(i - 1, j) else None
                | Right -> if j + 1 < n then Some(i, j + 1) else None
                | Down -> if i + 1 < n then Some(i + 1, j) else None
                | Left -> if j >= 1 then Some(i, j - 1) else None

            match nextPos with
            | Some(ni, nj) ->
                let ni, nj, nd =
                    if map[ni][nj] = '#' then
                        (i, j, dir.Next())
                    else
                        (ni, nj, dir)

                if history.Contains((ni, nj, nd)) then
                    foundLoop <- true
                else
                    history.Add((ni, nj, nd)) |> ignore
                    stack.Enqueue((ni, nj, nd))
            | None -> ()

        foundLoop

    let si, sj =
        List.allPairs [ 0 .. (n - 1) ] [ 0 .. (n - 1) ]
        |> List.find (fun (i, j) -> map[i][j] = '^')

    let results =
        List.allPairs [ 0 .. (n - 1) ] [ 0 .. (n - 1) ]
        |> List.toArray
        |> Array.Parallel.map (fun (i, j) ->
            if map[i][j] = '.' then
                let row = map[i] |> Array.updateAt j '#'
                let newMap = map |> Array.updateAt i row
                findLoop newMap (si, sj) Up
            else
                false)

    results |> Array.filter id |> Array.length


let parse (input: string) =
    input.Split([| '\r'; '\n' |], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun row -> row.Trim().ToCharArray())

module Example = 
    let input = 
        "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."

    [<Fact>]
    let testPart1 () = parse input |> part1 |> should equal 41

    [<Fact>]
    let testPart2 () = parse input |> part2 |> should equal 6 

open System.Diagnostics

[<EntryPoint>]
let main _ = 
    let input = stdin.ReadToEnd().TrimEnd()
    let map = parse input 

    let stopwatch = Stopwatch.StartNew()

    let part1Result = map |> part1
    printfn $"Part 1: %d{part1Result}"

    let part2Result = map |> part2
    printfn $"Part 2: %d{part2Result}"

    stopwatch.Stop()
    printfn $"Elapsed time: %A{stopwatch.Elapsed}"

    0