module day15

open NUnit.Framework
open FsUnit
open System.Text
open System.Diagnostics

type Cell =
    | Robot = 0
    | Box = 1
    | Wall = 2
    | Empty = 3

type Dir =
    | Up
    | Left
    | Down
    | Right

let inline swap (i, j) (a: 'T[]) =
    let temp = a[i]
    a[i] <- a[j]
    a[j] <- temp


let moveWithBox (map: Cell[][]) (ri: int) (rj: int) (di: int) (dj: int) =
    let rec moveBoxTail (ri: int) (rj: int) (di: int) (dj: int) (cont: bool -> bool) =
        let ni, nj = ri + di, rj + dj

        if ni < 0 || ni >= map.Length || nj < 0 || nj >= map[0].Length then
            cont false
        elif map[ni][nj] = Cell.Wall then
            cont false
        elif map[ni][nj] = Cell.Empty then
            map[ni][nj] <- Cell.Box
            map[ri][rj] <- Cell.Empty
            cont true
        elif map[ni][nj] = Cell.Box then
            moveBoxTail ni nj di dj (fun result ->
                if result then
                    map[ni][nj] <- Cell.Box
                    map[ri][rj] <- Cell.Empty
                    cont true
                else
                    cont false)
        else
            cont false

    moveBoxTail ri rj di dj id

let moveRobot (map: Cell[][]) (ri: int) (rj: int) (di: int) (dj: int) =
    let ni, nj = ri + di, rj + dj

    if ni < 0 || ni >= map.Length || nj < 0 || nj >= map[0].Length then
        ri, rj
    elif map[ni][nj] = Cell.Wall then
        ri, rj
    elif map[ni][nj] = Cell.Empty then
        map[ni][nj] <- Cell.Robot
        map[ri][rj] <- Cell.Empty
        ni, nj
    elif map[ni][nj] = Cell.Box then
        if moveWithBox map ni nj di dj then
            map[ni][nj] <- Cell.Robot
            map[ri][rj] <- Cell.Empty
            ni, nj
        else
            ri, rj
    else
        ri, rj

let findRobot (map: Cell[][]) =
    let mutable ri, rj = -1, -1

    for i = 0 to map.Length - 1 do
        for j = 0 to map[0].Length - 1 do
            if map[i][j] = Cell.Robot then
                ri <- i
                rj <- j

    ri, rj

let part1 ((map: Cell[][], moves: Dir[])) =
    let mutable ri, rj = findRobot map

    for move in moves do
        let di, dj =
            match move with
            | Up -> -1, 0
            | Down -> 1, 0
            | Left -> 0, -1
            | Right -> 0, 1

        let newRi, newRj = moveRobot map ri rj di dj
        ri <- newRi
        rj <- newRj

    let mutable sum = 0

    for i = 0 to map.Length - 1 do
        for j = 0 to map[0].Length - 1 do
            if map[i][j] = Cell.Box then
                sum <- sum + (100 * i + j)

    sum




let parseMap (input: string) =
    input.Split('\n', System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun row ->
        row.ToCharArray()
        |> Array.map (function
            | '@' -> Cell.Robot
            | 'O' -> Cell.Box
            | '#' -> Cell.Wall
            | '.' -> Cell.Empty
            | c -> failwith $"Unexpected map char: {c}"))


let parse (input: string) =
    let trimmed = input.Replace("\r", "")

    let parts =
        trimmed.Split([| "\n\n" |], System.StringSplitOptions.RemoveEmptyEntries)

    match parts with
    | [| mapPart; movesPart |] ->
        let map = parseMap mapPart

        let moves =
            movesPart.Split('\n', System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.collect (fun line ->
                line.Trim().ToCharArray()
                |> Array.map (function
                    | '^' -> Up
                    | '<' -> Left
                    | 'v' -> Down
                    | '>' -> Right
                    | c -> failwith $"Unknown move: {c}"))

        map, moves
    | _ -> failwith "Invalid input format. Expected two sections (map, moves) separated by a blank line."


[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let map, moves = parse input

    let stopwatch = Stopwatch.StartNew()

    (map, moves) |> part1 |> printfn "Part 1: %d"

    stopwatch.Stop()
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"

    0
