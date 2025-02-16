open Xunit
open FsUnit.Xunit
open System.Diagnostics

/// <summary>
/// Garden Groups - Day 12 Solution
/// This module solves the puzzle of calculating fencing costs for garden regions.
/// </summary>



/// Finds all connected garden plots of the same type starting from a given position
/// Returns a set of coordinates representing the entire region
let rec findConnectedGardenPlots (garden: char[][]) (row, col) visitedPlots =
    let directions = [(-1, 0); (0, -1); (1, 0); (0, 1)]  // up, left, down, right
    
    (Set.add (row, col) visitedPlots, directions)
    ||> List.fold (fun visited (deltaRow, deltaCol) ->
        let newRow, newCol = row + deltaRow, col + deltaCol
        
        // Check if the new position is valid and has same plant type
        if 0 <= newRow && newRow < garden.Length &&
           0 <= newCol && newCol < garden[row].Length &&
           garden[row][col] = garden[newRow][newCol] &&
           not (Set.contains (newRow, newCol) visited)
        then
            findConnectedGardenPlots garden (newRow, newCol) visited
        else
            visited)


/// Calculates the total fencing cost for all regions in the garden
let calculateTotalFencingCost (garden: char[][]) calculateRegionPrice = 
    let gardenSize = garden.Length
    // Verify garden is square
    garden |> Array.iter (fun row -> row.Length |> should equal gardenSize)

    // Process each plot and calculate costs
    (Set.empty, List.allPairs [0 .. (gardenSize - 1)] [0 .. (gardenSize - 1)])
    ||> List.mapFold (fun processedPlots (row, col) -> 
        if Set.contains (row, col) processedPlots then
            0, processedPlots  // Skip if already processed
        else
            let region = findConnectedGardenPlots garden (row, col) Set.empty
            calculateRegionPrice region, Set.union processedPlots region)
    |> fst
    |> List.sum

/// Part 1: Calculate fence price based on area × perimeter
let part1 (garden: char[][]) = 
    let calculateRegionPrice region =
        let area = Seq.length region
        
        // Calculate perimeter by counting exposed sides
        let perimeter =
            region
            |> Seq.sumBy (fun (row, col) ->
                [(-1, 0); (0, -1); (1, 0); (0, 1)]  // Check all four sides
                |> List.sumBy (fun (deltaRow, deltaCol) ->
                    let adjacentRow, adjacentCol = row + deltaRow, col + deltaCol
                    
                    if 0 <= adjacentRow && adjacentRow < garden.Length &&
                       0 <= adjacentCol && adjacentCol < garden[row].Length &&
                       garden[row][col] = garden[adjacentRow][adjacentCol]
                    then
                        0  // Side is connected to same plant type
                    else
                        1  // Side needs fencing
                ))
        
        area * perimeter

    calculateTotalFencingCost garden calculateRegionPrice

/// Part 2: Calculate fence price based on area × number of sides
let part2 (garden: char[][]) = 
    let calculateRegionPrice region = 
        let area = Seq.length region

        // Get all corner points of the region
        let cornerPoints = 
            region 
            |> Seq.collect (fun (row, col) -> 
                [(row, col); (row + 1, col); (row, col + 1); (row + 1, col + 1)])
            |> Set.ofSeq

        // Count number of sides by analyzing corner configurations
        let sideCount = 
            cornerPoints 
            |> Seq.sumBy (fun (row, col) -> 
                let surroundingPlots = 
                    Set.intersect 
                        (set [(row - 1, col - 1); (row - 1, col); 
                             (row, col - 1); (row, col)]) 
                        region
                
                let plotCount = Set.count surroundingPlots
                
                match plotCount with
                | 3 -> 1  // Three connected plots form one side
                | 1 -> 1  // Single plot forms one side
                | _ when surroundingPlots = set [(row - 1, col - 1); (row, col)] -> 2  // Diagonal connection
                | _ when surroundingPlots = set [(row - 1, col); (row, col - 1)] -> 2  // Other diagonal
                | _ -> 0)
            
        area * sideCount

    calculateTotalFencingCost garden calculateRegionPrice


let parse (input: string) =
    input.Split("\n") |> Array.map (fun row -> row.Trim().ToCharArray())



module Example =
    type Small() =
        let input =
            "AAAA
BBCD
BBCC
EEEC"

        [<Fact>]
        let testPart1 () =
            parse input |> part1 |> should equal 140

        [<Fact>]
        let testPart2 () = parse input |> part2 |> should equal 80

    type OX() =
        let input =
            "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO"

        [<Fact>]
        let testPart1 () =
            parse input |> part1 |> should equal 772

        [<Fact>]
        let testPart2 () =
            parse input |> part2 |> should equal 436

    type EX() =
        let input =
            "EEEEE
EXXXX
EEEEE
EXXXX
EEEEE"

        [<Fact>]

        let testPart2 () =
            parse input |> part2 |> should equal 236

    type AB() =
        let input =
            "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA"

        [<Fact>]

        let testPart2 () =
            parse input |> part2 |> should equal 368

    type Large() =
        let input =
            "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"

        [<Fact>]
        let testPart1 () =
            parse input |> part1 |> should equal 1930

        [<Fact>]
        let testPart2 () =
            parse input |> part2 |> should equal 1206



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