/// <summary>
/// Solves Advent of Code Day 21 challenge about finding optimal paths between keypad buttons.
/// </summary>
/// <remarks>
/// <para>The solution uses dynamic programming with memoization and path optimization techniques.</para>
/// <para>Problem details:</para>
/// <list type="bullet">
///   <item><description><b>Input:</b> A set of button sequences (codes) that need to be navigated</description></item>
///   <item><description><b>Part 1:</b> Calculate the minimum cost to navigate each sequence with a recursion level of 3</description></item>
///   <item><description><b>Part 2:</b> Calculate the minimum cost with a much deeper recursion level of 26</description></item>
/// </list>
/// <para>Both numeric (characters '0'-'9') and directional keypads are supported.</para>
/// <para>Directional keypad includes the following characters:</para>
/// <list type="bullet">
///   <item><description>'^' (up arrow)</description></item>
///   <item><description>'v' (down arrow)</description></item>
///   <item><description>'&lt;' (left arrow)</description></item>
///   <item><description>'&gt;' (right arrow)</description></item>
///   <item><description>'A' (action button)</description></item>
/// </list>
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
let getDoorKeypadPosition button =
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
    | c -> failwithf $"Invalid door keypad button: %c{c}"




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
let getDirectionalKeypadPosition button =
    match button with
    | '^' -> (0, 1)
    | 'A' -> (0, 2)
    | '<' -> (1, 0)
    | 'v' -> (1, 1)
    | '>' -> (1, 2)
    | c -> failwithf $"Invalid directional keypad button: %c{c}"




/// <summary>
/// Calculates possible routes between two numeric buttons on the keypad.
/// </summary>
///
/// <remarks>
/// For most button pairs, there are two possible routes:
/// 1. Moving vertically first, then horizontally (verticalFirst)
/// 2. Moving horizontally first, then vertically (horizontalFirst)
///
/// Special cases exist for certain button combinations where only one route is logical.
/// Each route ends with the 'A' button, representing the selection/confirmation.
/// </remarks>
///
/// <param name="sourceButton">The starting numeric button character</param>
/// <param name="targetButton">The destination numeric button character</param>
/// <returns>A list of possible routes, where each route is a list of buttons to press</returns>
let calculateNumericButtonRoute sourceButton targetButton =
    let sourceRow, sourceCol = getDoorKeypadPosition sourceButton
    let targetRow, targetCol = getDoorKeypadPosition targetButton
    
    let verticalMoves = List.init (abs (sourceRow - targetRow)) (fun _ -> 
        if sourceRow < targetRow then 'v' else '^')
    
    let horizontalMoves = List.init (abs (sourceCol - targetCol)) (fun _ -> 
        if sourceCol < targetCol then '>' else '<')
    
    let verticalFirst = verticalMoves @ horizontalMoves @ ['A'] 
    let horizontalFirst = horizontalMoves @ verticalMoves @ ['A']

    // Special cases for bottom row to left column transitions (and vice versa)
    // where one route is more logical than having two options
    let bottomRowButtons = ['0'; 'A']
    let leftColButtons = ['7'; '4'; '1']

    match sourceButton, targetButton with
    | src, tgt when List.contains (src, tgt) (List.allPairs bottomRowButtons leftColButtons) -> 
        [verticalFirst]
    | src, tgt when List.contains (src, tgt) (List.allPairs leftColButtons bottomRowButtons) -> 
        [horizontalFirst]
    | _ -> 
        List.distinct [verticalFirst; horizontalFirst]




/// <summary>
/// Calculates possible routes between two directional buttons on the keypad.
/// </summary>
///
/// <remarks>
/// For most button pairs, there are two possible routes:
/// 1. Moving vertically first, then horizontally (verticalFirst)
/// 2. Moving horizontally first, then vertically (horizontalFirst)
///
/// Special cases exist for certain button combinations where only one route is logical.
/// Each route ends with the 'A' button, representing the selection/confirmation.
/// </remarks>
///
/// <param name="sourceButton">The starting directional button character</param>
/// <param name="targetButton">The destination directional button character</param>
/// <returns>A list of possible routes, where each route is a list of buttons to press</returns>
let calculateDirectionalButtonRoute sourceButton targetButton =
    let sourceRow, sourceCol = getDirectionalKeypadPosition sourceButton
    let targetRow, targetCol = getDirectionalKeypadPosition targetButton
    
    let verticalMoves = List.init (abs (sourceRow - targetRow)) (fun _ -> 
        if sourceRow < targetRow then 'v' else '^')
    
    let horizontalMoves = List.init (abs (sourceCol - targetCol)) (fun _ -> 
        if sourceCol < targetCol then '>' else '<')
    
    let verticalFirst = verticalMoves @ horizontalMoves @ ['A'] 
    let horizontalFirst = horizontalMoves @ verticalMoves @ ['A']

    // Special cases for top row to left column transitions (and vice versa)
    let topRowButtons = ['^'; 'A']
    let leftColButtons = ['<']

    match sourceButton, targetButton with
    | src, tgt when List.contains (src, tgt) (List.allPairs topRowButtons leftColButtons) -> 
        [verticalFirst]
    | src, tgt when List.contains (src, tgt) (List.allPairs leftColButtons topRowButtons) -> 
        [horizontalFirst]
    | _ -> 
        List.distinct [verticalFirst; horizontalFirst]



/// <summary>
/// Recursively calculates the minimum cost path between two buttons with a specified recursion level.
/// </summary>
///
/// <remarks>
/// This function recursively calculates the optimal path between two buttons on a keypad,
/// considering both numeric and directional keypads. The recursion level determines how deep
/// the algorithm will analyze possible paths, with higher levels potentially finding more
/// optimal routes at the cost of increased computation.
/// </remarks>
///
/// <param name="recursionLevel">The recursion depth for path calculation</param>
/// <param name="sourceButton">The starting button character</param>
/// <param name="targetButton">The destination button character</param>
/// <returns>A list of button presses representing the minimum cost path</returns>
let rec calculateMinimumCostPath recursionLevel sourceButton targetButton =
    if recursionLevel = 0 then
        [targetButton]
    else
        // Determine if we're using the numeric keypad based on button characters
        let isNumericKeypad = 
            '0' <= sourceButton && sourceButton <= '9' || 
            '0' <= targetButton && targetButton <= '9'
            
        let possibleRoutes = 
            if isNumericKeypad then 
                calculateNumericButtonRoute sourceButton targetButton
            else 
                calculateDirectionalButtonRoute sourceButton targetButton

        possibleRoutes
        |> List.map (fun route ->
            'A' :: route // Prepend 'A' to represent the initial selection
            |> List.pairwise
            |> List.collect (fun (currentButton, nextButton) -> 
                calculateMinimumCostPath (recursionLevel - 1) currentButton nextButton))
        |> List.minBy List.length




/// <summary>
/// Dictionary for memoizing results of the minCost function to avoid redundant calculations.
/// </summary>
let memo = Dictionary<int * char * char, int64>()



/// <summary>
/// Recursively calculates the minimum cost between two buttons with a specified recursion level.
/// </summary>
///
/// <remarks>
/// This function calculates the minimum cost to navigate between two buttons on a keypad,
/// using memoization to improve performance by caching previously calculated results.
/// The recursion level determines how deep the algorithm will analyze possible paths.
/// </remarks>
///
/// <param name="recursionLevel">The recursion depth for cost calculation</param>
/// <param name="sourceButton">The starting button character</param>
/// <param name="targetButton">The destination button character</param>
/// <returns>The minimum cost (number of button presses) to navigate from source to target</returns>
let rec calculateMinimumCost recursionLevel sourceButton targetButton =
    let memoizationKey = (recursionLevel, sourceButton, targetButton)

    match memo.TryGetValue(memoizationKey) with
    | true, cachedResult -> cachedResult
    | false, _ ->
        let cost =
            if recursionLevel = 0 then
                1L // Base case: cost is 1 for directly pressing a button
            else
                // Determine if we're using the numeric keypad based on button characters
                let isNumericKeypad = 
                    '0' <= sourceButton && sourceButton <= '9' || 
                    '0' <= targetButton && targetButton <= '9'
                
                let possibleRoutes = 
                    if isNumericKeypad then 
                        calculateNumericButtonRoute sourceButton targetButton
                    else 
                        calculateDirectionalButtonRoute sourceButton targetButton

                possibleRoutes
                |> List.map (fun route ->
                    'A' :: route // Prepend 'A' to represent the initial selection
                    |> List.pairwise
                    |> List.sumBy (fun (currentButton, nextButton) -> 
                        calculateMinimumCost (recursionLevel - 1) currentButton nextButton))
                |> List.min

        memo.Add(memoizationKey, cost)
        cost




/// <summary>
/// Solves part 1 of the keypad navigation challenge.
/// </summary>
///
/// <remarks>
/// This function calculates the total cost for navigating a sequence of button codes,
/// using a recursion level of 3 for path calculation. Each code's cost is multiplied 
/// by the numeric value of the code (excluding any trailing 'A' characters).
/// </remarks>
///
/// <param name="keyCodes">A sequence of keypad button codes to navigate</param>
/// <returns>The total weighted cost of navigating all codes</returns>
let part1 (keyCodes: string seq) =
    keyCodes
    |> Seq.sumBy (fun keyCode ->
        let navigationCost =
            'A' :: List.ofSeq keyCode // Start from the 'A' button
            |> List.pairwise
            |> List.collect (fun (currentButton, nextButton) -> 
                calculateMinimumCostPath (2 + 1) currentButton nextButton) // Recursion level 3
            |> List.length
            |> int64

        let codeValue = keyCode.TrimEnd 'A' |> int64
        navigationCost * codeValue)



/// <summary>
/// Solves part 2 of the keypad navigation challenge.
/// </summary>
///
/// <remarks>
/// This function calculates the total cost for navigating a sequence of button codes,
/// using a recursion level of 26 for cost calculation. Each code's cost is multiplied 
/// by the numeric value of the code (excluding any trailing 'A' characters).
/// This is a more computationally intensive version compared to part 1.
/// </remarks>
///
/// <param name="keyCodes">A sequence of keypad button codes to navigate</param>
/// <returns>The total weighted cost of navigating all codes</returns>
let part2 (keyCodes: string seq) =
    keyCodes
    |> Seq.sumBy (fun keyCode ->
        let navigationCost =
            'A' :: List.ofSeq keyCode // Start from the 'A' button
            |> List.pairwise
            |> List.sumBy (fun (currentButton, nextButton) -> 
                calculateMinimumCost (25 + 1) currentButton nextButton) // Recursion level 26

        let codeValue = keyCode.TrimEnd 'A' |> int64
        navigationCost * codeValue)



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
