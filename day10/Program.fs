module Day10

open NUnit.Framework
open FsUnit
open System.Diagnostics


let part1 (map: int[][]) =
    let n = map.Length
    map |> Array.iter (fun row -> row.Length |> should equal n)

    let rec dfs (i, j) =
        (set [ (i, j) ], [ (-1, 0); (0, -1); (1, 0); (0, 1) ])
        ||> List.fold (fun acc (di, dj) ->
            let ni, nj = i + di, j + dj

            if 0 <= ni && ni < n && 0 <= nj && nj < n && map[i][j] + 1 = map[ni][nj] then
                Set.union acc (dfs (ni, nj))
            else
                acc)

    List.allPairs [ 0 .. (n - 1) ] [ 0 .. (n - 1) ]
    |> List.filter (fun (i, j) -> map[i][j] = 0)
    |> List.sumBy (fun (i, j) -> 
        dfs (i, j) 
        |> Seq.filter (fun (i, j) -> map[i][j] = 9) 
        |> Seq.length)



let part2 (map: int[][]) =
    let n = map.Length
    map |> Array.iter (fun row -> row.Length |> should equal n)

    let rec dfs (i, j) =
        (set [ [ (i, j) ] ], [ (-1, 0); (0, -1); (1, 0); (0, 1) ])
        ||> List.fold (fun acc (di, dj) -> 
            let ni, nj = i + di, j + dj
            
            if 0 <= ni && ni < n && 0 <= nj && nj < n && map[i][j] + 1 = map[ni][nj] then
                dfs (ni, nj)
                |> Seq.map (fun trail -> (ni, nj) :: trail)
                |> Set.ofSeq
                |> Set.union acc
            else 
                acc)

    List.allPairs [ 0 .. (n - 1) ] [0 .. (n - 1) ]
    |> List.filter (fun (i, j) -> map[i][j] = 0)
    |> List.sumBy (fun (i, j) -> 
        dfs (i, j)
        |> Seq.filter (List.exists (fun (i, j) -> map[i][j] = 9))
        |> Seq.length)



let parse (input: string) =
    input.Split("\n")
    |> Array.map (fun row -> 
        row.Trim().ToCharArray() 
        |> Array.map (fun c -> int c - int '0'))


module Example = 
    let input = "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"

    [<Test>]
    let ``testpart1`` () = parse input |> part1 |> should equal 36

    [<Test>]
    let ``testpart2`` () = parse input |> part2 |> should equal 81

[<EntryPoint>]
let main _ = 
    let input = stdin.ReadToEnd().TrimEnd()
    let map = parse input

    let stopwatch = Stopwatch.StartNew()

    map |> part1 |> printfn "Part 1: %d"
    map |> part2 |> printfn "Part 2: %d"

    stopwatch.Stop()
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"

    0