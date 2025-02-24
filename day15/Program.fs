module day15

open NUnit.Framework
open FsUnit
open System.Text
open System.Diagnostics

type Cell = 
    | Robot
    | Box
    | Wall
    | Empty

type Dir = 
    | Up
    | Left
    | Down
    | Right

let swap (i, j) (a: 'T[]) =
    let x, y = a[i], a[j]
    a |> Array.updateAt i y |> Array.updateAt j x


let transpose (a: 'T[][]) = 
    let h, w = a.Length, a[0].Length
    Array.init w (fun i -> Array.init h (fun j -> a[j][i]))


let findRobot (map: Cell[][]) =
    List.allPairs [ 0 .. (map.Length - 1) ] [ 0 .. (map[0].Length - 1) ]
    |> List.find (fun (i, j) -> map[i][j] = Robot)


let pushLeft (i, j) (map: Cell[][]) =
    let rec pushLeftTail (i, j) (map: Cell[][]) cont =
        assert (map[i][j] = Box)
        
        match map[i][j - 1] with
        | Wall -> None
        | Empty ->
            let newRow = map[i] |> swap (j - 1, j)
            map |> Array.updateAt i newRow |> Some |> cont
        | Box ->
            pushLeftTail (i, j - 1) map (fun newMapOpt ->
                match newMapOpt with
                | None -> None
                | Some newMap ->
                    assert (newMap[i][j - 1] = Empty)
                    pushLeftTail (i, j) newMap cont)
        | c -> failwithf $"%A{c} !?"
    
    pushLeftTail (i, j) map id

let moveLeft (map: Cell[][]) =
    let rec moveLeftTail map cont =
        let ri, rj = findRobot map
        
        match map[ri][rj - 1] with
        | Wall -> map |> cont
        | Empty ->
            let newRow = map[ri] |> swap (rj - 1, rj)
            map |> Array.updateAt ri newRow |> cont
        | Box ->
            match pushLeft (ri, rj - 1) map with
            | None -> map |> cont
            | Some newMap ->
                assert (newMap[ri][rj - 1] = Empty)
                moveLeftTail newMap cont
        | Robot -> failwith "!?"
    
    moveLeftTail map id


let moveRight (map: Cell[][]) = 
    let reverse (a: 'T[][]) = Array.map Array.rev a
    map |> reverse |> moveLeft |> reverse

let moveUp (map: Cell[][]) =
    map |> transpose |> moveLeft |> transpose


let moveDown (map: Cell[][]) =
    map |> transpose |> moveRight |> transpose


let part1 ((map, moves) : Cell[][] * Dir seq) =
    let map =
        (map, moves)
        ||> Seq.fold (fun map dir -> 
            let mv =
                match dir with
                | Up -> moveUp
                | Left -> moveLeft
                | Down -> moveDown
                | Right -> moveRight
                
            mv map)
    
    List.allPairs [ 0 .. (map.Length - 1) ] [ 0 .. (map[0].Length - 1) ]
    |> List.sumBy (fun (i, j) -> if map[i][j] = Box then 100 * i + j else 0)


let parseMap (input: string) =
    input.Split('\n', System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun row ->
        row.ToCharArray()
        |> Array.map (function
            | '@' -> Robot
            | 'O' -> Box
            | '#' -> Wall
            | '.' -> Empty
            | c -> failwith $"Unexpected map char: {c}")
    )


let parse (input: string) =
    let trimmed = input.Replace("\r", "")
    let parts = trimmed.Split([| "\n\n" |], System.StringSplitOptions.RemoveEmptyEntries)
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
                    | c -> failwith $"Unknown move: {c}")
            )
        map, moves
    | _ ->
        failwith "Invalid input format. Expected two sections (map, moves) separated by a blank line."


[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let map, moves = parse input

    let stopwatch = Stopwatch.StartNew()

    (map, moves) |> part1 |> printfn "Part 1: %d"

    stopwatch.Stop()
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds" 

    0