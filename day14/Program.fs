module Day14

open NUnit.Framework
open FsUnit
open System.Text
open System.Diagnostics

/// <summary>
/// Represents a robot with its position and velocity in a 2D grid
/// </summary>
type Robot = {
    /// Position as (x,y) coordinates where (0,0) is top-left corner
    Position: int * int
    /// Velocity as (dx,dy) where positive dx is right and positive dy is down
    Velocity: int * int
}

/// <summary>
/// Represents the four quadrants of the bathroom grid
/// </summary>
type BathroomQuadrant =
    | TopRight    // x > width/2  && y < height/2
    | TopLeft     // x < width/2  && y < height/2
    | BottomLeft  // x < width/2  && y > height/2
    | BottomRight // x > width/2  && y > height/2




/// <summary>
/// Simulates robot movement with wrapping around grid edges
/// </summary>
/// <param name="secondsRemaining">Number of time steps remaining</param>
/// <param name="gridWidth">Width of the grid</param>
/// <param name="gridHeight">Height of the grid</param>
/// <param name="robot">Robot to simulate</param>
/// <returns>Final robot state after simulation</returns>
let rec simulateRobotMovement secondsRemaining gridWidth gridHeight (robot: Robot) =
    if secondsRemaining = 0 then 
        robot 
    else 
        let posX, posY = robot.Position
        let velX, velY = robot.Velocity

        // Handle wrapping around grid edges using modulo
        let teleportedPosition = 
            (gridWidth + posX + velX) % gridWidth,   // Wrap X coordinate
            (gridHeight + posY + velY) % gridHeight   // Wrap Y coordinate
        
        simulateRobotMovement
            (secondsRemaining - 1)
            gridWidth
            gridHeight
            { robot with Position = teleportedPosition }




/// <summary>
/// Calculates safety factor by multiplying robot counts in each quadrant
/// </summary>
/// <param name="input">Tuple of (robots, grid width, grid height)</param>
/// <returns>Product of robot counts in each quadrant</returns>
let part1 ((securityRobots, gridWidth, gridHeight): Robot seq * int * int) =
    let quadrantCounts =
        securityRobots 
        |> Seq.map (simulateRobotMovement 100 gridWidth gridHeight)
        |> Seq.choose (fun { Position = (posX, posY)} -> 
            // Determine which quadrant the robot is in, excluding border lines
            match posX, posY with
            | x, y when x > gridWidth/2  && y < gridHeight/2 -> Some TopRight
            | x, y when x < gridWidth/2  && y < gridHeight/2 -> Some TopLeft
            | x, y when x < gridWidth/2  && y > gridHeight/2 -> Some BottomLeft
            | x, y when x > gridWidth/2  && y > gridHeight/2 -> Some BottomRight
            | _ -> None) // Robots on dividing lines don't count

    // Calculate safety factor by multiplying counts
    (1, Seq.countBy id quadrantCounts)
    ||> Seq.fold (fun acc (_, count) -> acc * count)




/// <summary>
/// Visualizes robot positions and finds the first valid cycle for Easter egg pattern
/// </summary>
/// <param name="input">Tuple of (robot list, grid width, grid height)</param>
/// <returns>Time steps needed for Easter egg pattern</returns>
let part2 ((robots: Robot list, w: int, h: int)) =
    /// <summary>Recursive function to visualize robot positions over time</summary>
    let rec search elapsed robots =
        if elapsed >= 1000 then
            ()
        else
            // Get current positions of all robots
            let positions = robots |> List.map (fun r -> r.Position) |> Set.ofList

            // Create and populate visualization grid
            let map = Array2D.create h w ' '
            positions |> Set.iter (fun (row, col) -> 
                if row >= 0 && row < h && col >= 0 && col < w then 
                    map.[row, col] <- '@')

            // Display current state
            let sb = StringBuilder()
            for i = 0 to h-1 do
                for j = 0 to w-1 do
                    sb.Append(map.[i,j]) |> ignore
                sb.AppendLine() |> ignore
            printfn "Time = %d\n%s" elapsed (sb.ToString())

            // Simulate next step
            search (elapsed + 1) (robots |> List.map (simulateRobotMovement 1 w h))

    // Start visualization
    search 0 robots

    /// <summary>Find first valid cycle for Easter egg pattern</summary>
    let rec findFirstValid p =
        let numQ = (81 - 30) + p * w
        if numQ % h = 0 then 
            81 + p * w
        else 
            findFirstValid (p + 1)
    
    findFirstValid 0







/// <summary>
/// Parses input string into Robot records
/// </summary>
/// <param name="input">Raw input string in format "p=x,y v=dx,dy"</param>
/// <returns>Array of Robot records</returns>
let parse (input: string) = 
    input.Trim().Replace("\r\n", "\n").Split("\n")
    |> Array.map (fun line ->
        try
            // Parse position and velocity components
            let parts = line.Split(" ")
            if parts.Length <> 2 then
                failwithf $"Invalid line format. Expected 2 parts, got {parts.Length}: %s{line}"
                
            let position, velocity = parts[0], parts[1]
            
            // Validate format
            if not (position.StartsWith("p=")) then
                failwithf $"Invalid position format. Expected 'p=', got: %s{position}"
            if not (velocity.StartsWith("v=")) then
                failwithf $"Invalid velocity format. Expected 'v=', got: %s{velocity}"
                
            // Extract coordinates
            let pos = position.Replace("p=", "").Split(",")
            let vel = velocity.Replace("v=", "").Split(",")
            
            // Validate coordinate pairs
            if pos.Length <> 2 || vel.Length <> 2 then
                failwithf $"Invalid coordinate format in: %s{line}"
                
            // Parse coordinate values
            let parseCoordinate (value: string) label =
                match System.Int32.TryParse(value) with
                | true, num -> num
                | false, _ -> failwithf $"Invalid {label} coordinate: %s{value}"
                
            // Create Robot record
            {
                Position = parseCoordinate pos.[0] "position X", parseCoordinate pos.[1] "position Y"
                Velocity = parseCoordinate vel.[0] "velocity X", parseCoordinate vel.[1] "velocity Y"
            }
        with e ->
            printfn $"\n\nError parsing line:\n%s{line}"
            raise e
    )




// Test module with example data
module Example =
    let input = """p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"""

    [<Test>]
    let ``Part 1 should calculate correct safety factor`` () =
        (parse input, 7, 11) |> part1 |> should equal 12


[<EntryPoint>]
let main _ = 
    let input = stdin.ReadToEnd().TrimEnd() 
    let robots = parse input

    let stopwatch = Stopwatch.StartNew()

    (robots, 101, 103) |> part1 |> printfn "Part 1: %d"
    (robots |> Array.toList, 101, 103) |> part2 |> printfn "Part 2: %d"

    stopwatch.Stop()
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"   

    0