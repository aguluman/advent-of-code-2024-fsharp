/// <summary>
/// Day 25: Code Chronicle - Virtual Five-Pin Tumbler Lock Analysis
/// </summary>
/// <description>
/// Solves the Advent of Code Day 25 challenge about analyzing virtual five-pin tumbler locks and keys.
/// The Historians are trying to access a locked office and need to determine which keys fit which locks.
/// <list type="bullet">
///   <item><description><b>Part 1:</b> Count all unique lock/key pairs that fit together without overlapping in any column</description></item>
/// </list>
/// </description>
/// <remarks>
/// <para>Problem details:</para>
/// <list type="bullet">
///   <item><description><b>Input:</b> Schematics of locks and keys represented as patterns of # and . characters</description></item>
///   <item><description><b>Locks:</b> Schematics with top row filled (#) and bottom row empty (.)</description></item>
///   <item><description><b>Keys:</b> Schematics with top row empty and bottom row filled</description></item>
///   <item><description><b>Compatibility:</b> A lock and key fit if their combined height in each column doesn't exceed the available space</description></item>
///   <item><description><b>Output (Part 1):</b> Number of unique lock/key pairs that fit together without overlapping</description></item>
/// </list>
/// 
/// <para>The challenge involves converting the visual schematics into numerical height representations and then testing each lock against each key to determine compatibility.</para>
/// 
/// See details at: <see href="https://adventofcode.com/2023/day/25">Advent of Code 2023, Day 25</see>
/// </remarks>

module day25

open System.Diagnostics
open NUnit.Framework
open FsUnit



/// <summary>
/// Solves part 1 of the challenge by calculating the number of unique lock/key pairs that fit together.
/// </summary>
/// <param name="schematics">A list of character arrays representing lock and key schematics.</param>
/// <returns>The count of unique lock/key pairs that fit together without overlapping.</returns>
/// <remarks>
/// <para>
/// The function works by:
/// </para>
/// <list type="number">
/// <item>
/// <description>Identifying locks (schematics with top row filled) and keys (schematics with top row empty)</description>
/// </item>
/// <item>
/// <description>Converting both to representations of their heights</description>
/// </item>
/// <item>
/// <description>Testing each lock with each key to check for compatible fits</description>
/// </item>
/// <item>
/// <description>Counting the number of valid combinations</description>
/// </item>
/// </list>
/// </remarks>
let part1 (schematics: char[][] list) =
    let h, w = schematics[0].Length, (schematics[0][0]).Length
   
    /// Extracts locks from the schematics by finding patterns with the top row filled.
    let locks =
        schematics
        |> List.filter (fun s -> s[0] |> Array.forall ((=) '#'))
        |> List.map (fun s ->
            [ 0 .. (w - 1) ]
            |> List.map (fun j -> [ 0 .. (h - 1) ] |> List.sumBy (fun i -> if s[i][j] = '.' then 0 else 1)))


    /// Extracts keys from the schematics by finding patterns with the top row empty.
    let keys =
        schematics
        |> List.filter (fun s -> s[0] |> Array.forall ((=) '.'))
        |> List.map (fun s ->
            [ 0 .. (w - 1) ]
            |> List.map (fun j -> [ 0 .. (h - 1) ] |> List.sumBy (fun i -> if s[i][j] = '.' then 0 else 1)))

    List.allPairs locks keys
    |> List.filter (fun (lock, key) -> List.zip lock key |> List.forall (fun (lock, key) -> lock + key <= h))
    |> List.length



/// <summary>
/// Parses the input string into a structured representation of lock and key schematics.
/// </summary>
/// <param name="input">The raw input string containing multiple schematics separated by blank lines.</param>
/// <returns>A list of character arrays representing individual schematics.</returns>
/// <remarks>
/// <para>
/// The function splits the input at blank lines to separate individual schematics,
/// then converts each line to a character array.
/// </para>
/// </remarks>
let parse (input: string) =
    input.Split([| "\n\n"; "\r\n\r\n" |], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun section ->
        section.Split([| "\n"; "\r\n" |], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun line -> line.TrimEnd().ToCharArray()))
    |> Array.toList



/// <summary>
/// Contains example data and test cases for the solution.
/// </summary>
module Example =
    /// <summary>
    /// Example input provided in the challenge description.
    /// </summary>
    let input =
        "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####"

    /// <summary>
    /// Tests the part1 function with the example input.
    /// </summary>
    /// <remarks>
    /// <para>
    /// According to the problem description, there should be 3 unique lock/key pairs
    /// that fit together without overlapping in any column.
    /// </para>
    /// </remarks>
    [<Test>]
    let testPart1 () = parse input |> part1 |> should equal 3



/// <summary>
/// Entry point for the application.
/// </summary>
/// <param name="_">Command-line arguments (unused).</param>
/// <returns>Exit code (0 indicates success).</returns>
/// <remarks>
/// <para>
/// The main function:
/// </para>
/// <list type="number">
/// <item>
/// <description>Reads the input from standard input</description>
/// </item>
/// <item>
/// <description>Parses the input into lock and key schematics</description>
/// </item>
/// <item>
/// <description>Executes part 1 of the solution and displays the result</description>
/// </item>
/// <item>
/// <description>Measures and reports execution time</description>
/// </item>
/// </list>
/// </remarks>
[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let schematics = parse input

    let stopwatch = Stopwatch()
    stopwatch.Start()

    schematics |> part1 |> printfn "Part 1: %d"

    stopwatch.Stop()

    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"

    0