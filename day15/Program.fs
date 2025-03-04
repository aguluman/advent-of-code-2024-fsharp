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
    a


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


module Part2 =
    type Cell2 =
        | Robot
        | BoxL
        | BoxR
        | Wall
        | Empty

    let downgrade =
        function
        | Robot -> Cell.Robot
        | BoxL
        | BoxR
        | Wall -> Cell.Wall
        | Empty -> Cell.Empty


    let toString (map: Cell2[][]) =
        map
        |> Array.map (fun row ->
            row
            |> Array.map (function
                | Robot -> '@'
                | BoxL -> 'O'
                | BoxR -> 'O'
                | Wall -> '#'
                | Empty -> '.')
            |> System.String)
        |> String.concat "\n"



    let findRobot (map: Cell2[][]) =
        map |> Array.map (fun row -> row |> Array.map downgrade) |> findRobot

    let rec pushLeft (i, j) (map: Cell2[][]) =
        assert (map[i][j] = BoxR)
        if j < 2 || map[i][j - 2] <> BoxL then
            None // Can't push because no BoxL in the expected position
        else

        match map[i][j - 2] with
        | Wall -> None
        | Empty ->
            // #..[].. -> #.[]...
            let newRow = map[i] |> swap (j - 2, j - 1) |> swap (j - 1, j)
            map |> Array.updateAt i newRow |> Some
        | BoxR ->
            pushLeft (i, j - 2) map
            |> Option.map (fun map ->
                assert (map[i][j - 2] = Empty)
                //delegate
                pushLeft (i, j) map |> Option.get)
        | c -> failwithf $"%A{c} !?"



    let rec pushUp (i, j) (map: Cell2[][]) =
        match map[i][j] with
        | BoxL ->
            assert (map[i][j + 1] = BoxR)

            match map[i - 1][j], map[i - 1][j + 1] with
            | Wall, _
            | _, Wall -> None
            | Empty, Empty ->
                let newRow1 = map[i - 1] |> Array.updateAt j BoxL |> Array.updateAt (j + 1) BoxR
                let newRow2 = map[i] |> Array.updateAt j Empty |> Array.updateAt (j + 1) Empty
                map |> Array.updateAt (i - 1) newRow1 |> Array.updateAt i newRow2 |> Some
            // []
            | BoxL, BoxR
            // ].
            | BoxR, Empty ->
                pushUp (i - 1, j) map
                |> Option.map (fun map ->
                    assert (map[i - 1][j] = Empty)
                    assert (map[i - 1][j + 1] = Empty)
                    //delegate
                    pushUp (i, j) map |> Option.get)
            // .[
            | Empty, BoxL ->
                pushUp (i - 1, j + 1) map
                |> Option.map (fun map ->
                    assert (map[i - 1][j] = Empty)
                    assert (map[i - 1][j + 1] = Empty)
                    //delegate
                    pushUp (i, j) map |> Option.get)
            // ][
            | BoxR, BoxL ->
                pushUp (i - 1, j) map
                |> Option.bind (fun map ->
                    assert (map[i - 1][j - 1] = Empty)
                    assert (map[i - 1][j] = Empty)
                    pushUp (i - 1, j + 1) map)
                |> Option.map (fun map ->
                    assert (map[i - 1][j] = Empty)
                    assert (map[i - 1][j + 1] = Empty)
                    //delegate
                    pushUp (i, j) map |> Option.get)
            | c1, c2 -> failwithf $"%A{c1} %A{c2} !?"
        | BoxR ->
            //delegate
            pushUp (i, j - 1) map
        | c -> failwithf $"%A{c} !?"



    let rec moveLeft (map: Cell2[][]) =
        let ri, rj = findRobot map

        match map[ri][rj - 1] with
        | Wall -> map
        | Empty ->
            // #...@. -> #..@..
            let newRow = map[ri] |> swap (rj - 1, rj)
            map |> Array.updateAt ri newRow
        | BoxR ->
            map
            |> pushLeft (ri, rj - 1)
            |> Option.map (fun map ->
                assert (map[ri][rj - 1] = Empty)
                //delegate
                moveLeft map)
            |> Option.defaultWith (fun () -> map)
        | c -> failwithf $"%A{c} !?"

    let rec moveUp (map: Cell2[][]) =
        let ri, rj = findRobot map

        match map[ri - 1][rj] with
        | Wall -> map
        | Empty ->
            let newRow1 = map[ri - 1] |> Array.updateAt rj Robot
            let newRow2 = map[ri] |> Array.updateAt rj Empty
            map |> Array.updateAt (ri - 1) newRow1 |> Array.updateAt ri newRow2
        | BoxL
        | BoxR ->
            map
            |> pushUp (ri - 1, rj)
            |> Option.map (fun map ->
                assert (map[ri - 1][rj] = Empty)
                //delegate
                moveUp map)
            |> Option.defaultWith (fun () -> map)
        | c -> failwithf $"%A{c} !?"

    let moveRight (map: Cell2[][]) =
        let reverse (map: Cell2[][]) =
            map
            |> Array.map (fun row ->
                row
                |> Array.rev
                |> Array.map (function
                    | BoxL -> BoxR
                    | BoxR -> BoxL
                    | c -> c))

        map |> reverse |> moveLeft |> reverse

    let moveDown (map: Cell2[][]) = map |> Array.rev |> moveUp |> Array.rev

    let scaleUp (map: Cell[][]) =
        map
        |> Array.map (fun row ->
            row
            |> Array.collect (function
                | Cell.Robot -> [| Robot; Empty |]
                | Cell.Box -> [| BoxL; BoxR |]
                | Cell.Wall -> [| Wall; Wall |]
                | Cell.Empty -> [| Empty; Empty |]
                | _ -> System.ArgumentOutOfRangeException() |> raise))

    let part2 ((map, moves): (Cell[][] * Dir seq)) =
        let map =
            (scaleUp map, moves)
            ||> Seq.fold (fun map dir ->
                let mv =
                    match dir with
                    | Up -> moveUp
                    | Left -> moveLeft
                    | Down -> moveDown
                    | Right -> moveRight

                mv map)

        List.allPairs [ 0 .. (map.Length - 1) ] [ 0 .. (map[0].Length - 1) ]
        |> List.sumBy (fun (i, j) -> if map[i][j] = BoxL then i * 100 + j else 0)


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
    (map, moves) |> Part2.part2 |> printfn "Part 2: %d"

    stopwatch.Stop()
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"

    0
