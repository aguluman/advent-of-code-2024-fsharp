module Day08

open Xunit
open FsUnit.Xunit

let calculateAntinodePositions (gridSize: int) (startRow, startColumn) (endRow, endColumn) =
    if (startRow, startColumn) = (endRow, endColumn) then
        []
    else
        let rowStep, columnStep = endRow - startRow, endColumn - startColumn

        (endRow, endColumn)
        |> List.unfold (fun (currentRow, currentColumn) ->
            let nextRow, nextColumn = currentRow + rowStep, currentColumn + columnStep

            if nextRow >= 0 && nextRow < gridSize && nextColumn >= 0 && nextColumn < gridSize 
            then
                Some((nextRow, nextColumn), (nextRow, nextColumn))
            else
                None)

let solveGrid (grid: char[][]) positionMapper =
    let gridSize = grid.Length
    grid |> Array.iter (fun row -> row.Length |> should equal gridSize)

    [ '0' .. '9' ]
    |> List.append [ 'a' .. 'z' ]
    |> List.append [ 'A' .. 'Z' ]
    |> List.map (fun char ->
        let findCharPositions =
            List.allPairs [ 0 .. (gridSize - 1) ] [ 0 .. (gridSize - 1) ]
            |> List.filter (fun (row, column) -> grid[row][column] = char)

        let mappedPoints =
            List.allPairs findCharPositions findCharPositions
            |> List.collect (fun ((r1, c1), (r2, c2)) ->
                positionMapper gridSize (r1, c1) (r2, c2))

        set mappedPoints)

    |> Set.unionMany
    |> Set.count

let part1 (grid: char[][]) =
    solveGrid grid (fun size (r1, c1) (r2, c2) ->
        //only head
        match calculateAntinodePositions size (r1, c1) (r2, c2) with
        | [] -> []
        | firstPosition :: _ -> [ firstPosition ])

let part2 (grid: char[][]) =
    solveGrid grid (fun size (r1, c1) (r2, c2) ->
        let tail = calculateAntinodePositions size (r1, c1) (r2, c2)
        //It seems that the original antenna at (i', j') is also included in the count.
        (r2, c2) :: tail)

let parse (input: string) =
    input.Split([| '\r'; '\n' |], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun row -> row.Trim().ToCharArray())


module Example =
    let input =
        "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"

    [<Fact>]
    let testPart1 () = parse input |> part1 |> should equal 14

    [<Fact>]
    let testPart2 () = parse input |> part2 |> should equal 34

open System.Diagnostics
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let grid = parse input

    let stopwatch: Stopwatch = Stopwatch.StartNew()

    grid |> part1 |> printfn "Part 1: %d"
    grid |> part2 |> printfn "Part 2: %d"

    stopwatch.Stop()
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"

    0