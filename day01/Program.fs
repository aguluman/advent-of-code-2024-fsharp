module Day01

open FsUnit.Xunit
open Xunit

let part1 (locationIds: (int * int) seq) =
    let left, right = locationIds
                      |> List.ofSeq
                      |> List.unzip

    List.zip (List.sort left) (List.sort right)
    |> List.sumBy (fun (l, r) -> abs (l - r))


let part2 (locationIds: (int * int) seq) =
    let left, right = locationIds
                      |> List.ofSeq
                      |> List.unzip
    
    let counter = right
                  |>List.countBy id
                  |> Map.ofList
    
    left
    |> List.sumBy (fun l ->
        let c =
            match Map.tryFind l counter with
            | Some c -> c
            | None -> 0
    
        (int64 l) * (int64 c))
    
    
let parse (input: string) =
    input.Split("\n")
    |> Seq.map (fun line ->
        let line = line.Split("   ")
        (int line[0], int line[1]))
    
    
module Example =
    let input =
        "3   4
 4   3
 2   5
 1   3
 3   9
 3   3"
         
    [<Fact>]
    let testPart1() = parse input |> part1 |> should equal 11
    
    [<Fact>]
    let testPart2() = parse input |> part2 |> should equal 31L
    
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let locationIds = parse input
    
    locationIds
    |> part1
    |> printfn "Part 1: %d"
    
    locationIds
    |> part2
    |> printfn "Part 2: %d"
    
    0
    
//How to run your AOC test case

(*
# Run with input file
type input.txt | dotnet run

# Test with different input
type test-cases.txt | dotnet run
*)
