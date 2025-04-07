/// <summary>
/// Day 21: Keypad Navigation - Optimal Path Challenge
/// </summary>
/// <description>
/// Solves Advent of Code Day 21 challenge about finding optimal paths between keypad buttons.
/// The module calculates minimum-cost paths for navigating between buttons on numeric and directional keypads.
/// </description>
///
/// <remarks>
/// Problem details:
/// - Input: A set of button sequences (codes) that need to be navigated
/// - Part 1: Calculate the minimum cost to navigate each sequence with a recursion level of 3
/// - Part 2: Calculate the minimum cost with a much deeper recursion level of 26
///
/// <summary>
   /// The solution uses dynamic programming with memoization and path optimization techniques.
   /// </summary>
   /// <remarks>
   /// Both numeric (characters '0'-'9') and directional keypads are supported.
   /// Directional keypad includes the following characters:
   /// <list type="bullet">
   ///   <item><description>'^' (up arrow)</description></item>
   ///   <item><description>'v' (down arrow)</description></item>
   ///   <item><description>'&lt;' (left arrow)</description></item>
   ///   <item><description>'&gt;' (right arrow)</description></item>
   ///   <item><description>'A' (action button)</description></item>
   /// </list>
   /// </remarks>
/// See: <see href="https://adventofcode.com/2024/day/21">Advent of Code 2024, Day 21</see>
/// </remarks>
module day21

open System.Diagnostics
open System.Collections.Generic
open NUnit.Framework
open FsUnit


/// <summary>
/// Maps a numeric button character to its 2D position on the numeric keypad.
/// </summary>
///
/// <remarks>
/// The numeric keypad layout is as follows:
/// 
/// <code>
/// 7 8 9
/// 4 5 6
/// 1 2 3
///   0 A
/// </code>
/// 
/// </remarks>
///
/// <param name="button">A character representing a button on the numeric keypad</param>
/// <returns>A tuple (row, column) representing the button's position</returns>
let numPos button =
    match button with
    | '7' -> (0, 0)
    | '8' -> (0, 1)
    | '9' -> (0, 2)
    | '4' -> (1, 0)
    | '5' -> (1, 1)
    | '6' -> (1, 2)
    | '1' -> (2, 0)
    | '2' -> (2, 1)
    | '3' -> (2, 2)
    | '0' -> (3, 1)
    | 'A' -> (3, 2)
    | c -> failwithf $"%c{c} !?"


/// <summary>
/// Retrieves the row and column coordinates for a specified directional keypad button.
/// </summary>
/// <remarks>
/// The directional keypad layout looks like this:
///
/// <code>
///     ^  A
///  &lt;  v  &gt;
/// </code>
/// 
/// Accepted characters include '^', 'A', '&lt;', 'v', and '&gt;'. Any unrecognized character
/// will cause an exception to be thrown.
/// </remarks>
/// <param name="button">
/// A single character representing the directional keypad button.
/// </param>
/// <returns>
/// A tuple (row, column) indicating the button's position in a 2D grid,
/// where row 0, column 0 is the top-left corner.
/// </returns>
/// <exception cref="System.Exception">
/// Thrown if the specified <paramref name="button"/> is not a valid directional button.
/// </exception>
let dirPos button =
    match button with
    | '^' -> (0, 1)
    | 'A' -> (0, 2)
    | '<' -> (1, 0)
    | 'v' -> (1, 1)
    | '>' -> (1, 2)
    | c -> failwithf $"%c{c} !?"


/// <summary>
/// Calculates possible routes between two numeric buttons on the keypad.
/// </summary>
///
/// <remarks>
/// For most button pairs, there are two possible routes:
/// 1. Moving vertically first, then horizontally (tate)
/// 2. Moving horizontally first, then vertically (yoko)
///
/// Special cases exist for certain button combinations where only one route is logical.
/// Each route ends with the 'A' button, representing the selection/confirmation.
/// </remarks>
///
/// <param name="num1">The starting button character</param>
/// <param name="num2">The destination button character</param>
/// <returns>A list of possible routes, where each route is a list of buttons to press</returns>
let numRoute num1 num2 =
    let xi, xj = numPos num1
    let yi, yj = numPos num2
    let di = List.init (abs (xi - yi)) (fun _ -> if xi < yi then 'v' else '^')
    let dj = List.init (abs (xj - yj)) (fun _ -> if xj < yj then '>' else '<')
    let tate = di @ dj @ [ 'A' ] // Vertical first route
    let yoko = dj @ di @ [ 'A' ] // Horizontal first route

    // Special cases for bottom row to left column transitions (and vice versa)
    // where one route is more logical than having two options
    let bottom = [ '0'; 'A' ]
    let left = [ '7'; '4'; '1' ]

    match num1, num2 with
    | num1, num2 when List.contains (num1, num2) (List.allPairs bottom left) -> [ tate ]
    | num1, num2 when List.contains (num1, num2) (List.allPairs left bottom) -> [ yoko ]
    | _ -> List.distinct [ tate; yoko ]


/// <summary>
/// Calculates possible routes between two directional buttons on the keypad.
/// </summary>
///
/// <remarks>
/// Similar to numRoute, but for the directional keypad layout.
/// For most button pairs, there are two possible routes, but special cases exist
/// for certain combinations where only one route makes sense.
/// </remarks>
///
/// <param name="dir1">The starting directional button character</param>
/// <param name="dir2">The destination directional button character</param>
/// <returns>A list of possible routes, where each route is a list of buttons to press</returns>
let dirRoute dir1 dir2 =
    let x1, xj = dirPos dir1
    let y1, yj = dirPos dir2
    let di = List.init (abs (x1 - y1)) (fun _ -> if x1 < y1 then 'v' else '^')
    let dj = List.init (abs (xj - yj)) (fun _ -> if xj < yj then '>' else '<')
    let tate = di @ dj @ [ 'A' ] // Vertical first route
    let yoko = dj @ di @ [ 'A' ] // Horizontal first route

    // Special cases for top row to left column transitions (and vice versa)
    let top = [ '^'; 'A' ]
    let left = [ '<' ]

    match dir1, dir2 with
    | dir1, dir2 when List.contains (dir1, dir2) (List.allPairs top left) -> [ tate ]
    | dir1, dir2 when List.contains (dir1, dir2) (List.allPairs left top) -> [ yoko ]
    | _ -> List.distinct [ tate; yoko ]


/// <summary>
/// Recursively finds the minimum-length path between two buttons with a given recursion level.
/// </summary>
///
/// <remarks>
/// This function implements a recursive path-finding algorithm:
/// - At level 0, the path is simply the destination button
/// - Otherwise, it finds all possible routes between the buttons and
///   recursively calculates the minimum path for each step in each route
///
/// The keypad type (numeric or directional) is determined by examining the button characters.
/// </remarks>
///
/// <param name="level">The recursion level, controlling the complexity of path finding</param>
/// <param name="b1">The starting button character</param>
/// <param name="b2">The destination button character</param>
/// <returns>A list of buttons representing the minimum-cost path</returns>
let rec minCostPath level b1 b2 =
    if level = 0 then
        [ b2 ]
    else
        // Determine if we're using the numeric keypad based on button characters
        let numKeypad = '0' <= b1 && b1 <= '9' || '0' <= b2 && b2 <= '9'
        let route = if numKeypad then numRoute b1 b2 else dirRoute b1 b2

        route
        |> List.map (fun route ->
            'A' :: route // Prepend 'A' to represent the initial selection
            |> List.pairwise
            |> List.collect (fun (x, y) -> minCostPath (level - 1) x y))
        |> List.minBy List.length


/// <summary>
/// Dictionary for memoizing results of the minCost function to avoid redundant calculations.
/// </summary>
let memo = Dictionary<int * char * char, int64>()


/// <summary>
/// Recursively calculates the minimum cost to navigate from one button to another.
/// </summary>
///
/// <remarks>
/// Similar to minCostPath but returns the cost value rather than the path itself.
/// Uses memoization to avoid recalculating results for the same inputs, which is
/// critical for performance with higher recursion levels.
/// </remarks>
///
/// <param name="level">The recursion level, controlling the complexity of path finding</param>
/// <param name="b1">The starting button character</param>
/// <param name="b2">The destination button character</param>
/// <returns>The minimum cost (as a 64-bit integer) to navigate between the buttons</returns>
let rec minCost level b1 b2 =
    let key = (level, b1, b2)

    match memo.TryGetValue(key) with
    | true, value -> value
    | false, _ ->
        let value =
            if level = 0 then
                1L // Base case: cost is 1 for directly pressing a button
            else
                // Determine if we're using the numeric keypad based on button characters
                let numKeypad = '0' <= b1 && b1 <= '9' || '0' <= b2 && b2 <= '9'
                let route = if numKeypad then numRoute b1 b2 else dirRoute b1 b2

                route
                |> List.map (fun route ->
                    'A' :: route // Prepend 'A' to represent the initial selection
                    |> List.pairwise
                    |> List.sumBy (fun (x, y) -> minCost (level - 1) x y))
                |> List.min

        memo.Add(key, value)
        value


/// <summary>
/// Solves Part 1 of the challenge using a recursion level of 3.
/// </summary>
///
/// <remarks>
/// For each code, calculates the minimum path length between consecutive buttons
/// and multiplies the total cost by the numeric value of the code.
/// </remarks>
///
/// <param name="codes">Sequence of button codes to process</param>
/// <returns>The total cost for all codes as defined by Part 1 rules</returns>
let part1 (codes: string seq) =
    codes
    |> Seq.sumBy (fun code ->
        let cost =
            'A' :: List.ofSeq code // Start from the 'A' button
            |> List.pairwise
            |> List.collect (fun (x, y) -> minCostPath (2 + 1) x y) // Recursion level 3
            |> List.length
            |> int64

        let num = code.TrimEnd 'A' |> int64
        cost * num)


/// <summary>
/// Solves Part 2 of the challenge using a deeper recursion level of 26.
/// </summary>
///
/// <remarks>
/// Similar to Part 1 but with a much higher recursion level.
/// Uses the memoized minCost function to make this computationally feasible.
/// </remarks>
///
/// <param name="codes">Sequence of button codes to process</param>
/// <returns>The total cost for all codes as defined by Part 2 rules</returns>
let part2 (codes: string seq) =
    codes
    |> Seq.sumBy (fun code ->
        let cost =
            'A' :: List.ofSeq code // Start from the 'A' button
            |> List.pairwise
            |> List.sumBy (fun (x, y) -> minCost (25 + 1) x y) // Recursion level 26

        let num = code.TrimEnd 'A' |> int64
        cost * num)


/// <summary>
/// Parses the input string into a sequence of button codes.
/// </summary>
///
/// <remarks>
/// Simple parsing that splits the input by newlines and trims whitespace.
/// </remarks>
///
/// <param name="input">Raw input string</param>
/// <returns>Array of button code strings</returns>
let parse (input: string) = input.Split "\n" |> Array.map _.Trim()


/// <summary>
/// Example test cases and assertions for the Keypad Navigation solution
/// </summary>
module Example =
    /// <summary>Example button codes from the challenge description</summary>
    let input =
        "029A
980A
179A
456A
379A"

    /// <summary>Tests that Part 1 correctly calculates the total cost for the example codes</summary>
    [<Test>]
    let testPart1 () =
        parse input |> part1 |> should equal 126384L


/// <summary>Main entry point for the program</summary>
[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let codes = parse input

    let stopwatch = Stopwatch()
    stopwatch.Start()

    codes |> part1 |> printfn "Part 1: %d"
    codes |> part2 |> printfn "Part 2: %d"

    stopwatch.Stop()
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"

    0
