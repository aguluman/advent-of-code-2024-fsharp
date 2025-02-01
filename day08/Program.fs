open Xunit
open FsUnit.Xunit

let antinodePositions (n: int) (i, j) (i', j') =
    if (i, j) = (i', j') then
        []
    else
        let di, dj = i' - i, j' - j

        (i', j')
        |> List.unfold (fun (i', j') ->
            let ni, nj = i' + di, j' + dj

            if 0 <= ni && ni < n && 0 <= nj && nj < n then
                Some((ni, nj), (ni, nj))
            else
                None)

let solve (map: char[][]) mapping =
    let n = map.Length
    map |> Array.iter (fun row -> row.Length |> should equal n)

    [ '0' .. '9' ]
    |> List.append [ 'a' .. 'z' ]
    |> List.append [ 'A' .. 'Z' ]
    |> List.map (fun c ->
        let positions =
            List.allPairs [ 0 .. (n - 1) ] [ 0 .. (n - 1) ]
            |> List.filter (fun (i, j) -> map[i][j] = c)

        let newPositions =
            List.allPairs positions positions
            |> List.collect (fun ((i, j), (i', j')) -> mapping n (i, j) (i', j'))

        set newPositions)

    |> Set.unionMany
    |> Set.count

let part1 (map: char[][]) =
    solve map (fun n (i, j) (i', j') ->
        //only head
        match antinodePositions n (i, j) (i', j') with
        | [] -> []
        | h :: _ -> [ h ])
    
let part2 (map: char[][]) =
    solve map (fun n (i, j) (i', j') ->
        let t = antinodePositions n (i, j) (i', j')
        //It seems that the original antenna at (i', j') is also included in the count.
        (i', j') :: t)

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

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let map = parse input

    let stopwatch: Stopwatch = Stopwatch.StartNew()

    map |> part1 |> printfn "Part 1: %d"
    map |> part2 |> printfn "Part 2: %d"
    
    stopwatch.Stop()
    //printfn $"Elapsed time: %A{stopwatch.Elapsed}"
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds}"
    0
