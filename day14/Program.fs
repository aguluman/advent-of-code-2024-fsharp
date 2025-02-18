module Day14

open NUnit.Framework
open FsUnit
open System.Text.RegularExpressions
open System.Diagnostics

type Robot = {
    Position: int * int;
    Velocity: int * int
}

type BathroomQuadrant =
    | TopRight
    | TopLeft
    | BottomLeft
    | BottomRight


let rec simulateRobotMovement secondsRemaining gridWidth gridHeight (robot: Robot) =
    if 
        secondsRemaining = 0
    then 
        robot 
    else 
        let posX, posY = robot.Position
        let velX, velY = robot.Velocity

        let teleportedPosition = 
            (gridWidth + posX + velX) % gridWidth, //X axis
            (gridHeight + posY + velY) % gridHeight //Y axis
        
        simulateRobotMovement
            (secondsRemaining - 1)
            gridWidth
            gridHeight
            { robot with 
                Position = teleportedPosition }


let part1 ((securityRobots, gridWidth, gridHeight): (Robot seq * int * int)) =
    let quadrantCounts =
        securityRobots 
        |> Seq.map (simulateRobotMovement 100 gridWidth gridHeight)
        |> Seq.choose (fun { Position = (posX, posY)}-> 
            if posX > gridWidth / 2 && posY < gridHeight / 2 then
                Some TopRight
            elif posX < gridWidth / 2 && posY < gridHeight / 2 then
                Some TopLeft
            elif posX < gridWidth / 2 && posY > gridHeight / 2 then
                Some BottomLeft
            elif posX > gridWidth / 2 && posY > gridHeight / 2 then
                Some BottomRight
            else
                None) //Robots on dividing lines don't count.

    // Multiply counts from each quadrant to get the safety factor
    (1, Seq.countBy id quadrantCounts)
    ||> Seq.fold (fun acc (_, count) -> acc * count) 


let part2 ((robots, w, h): (Robot seq * int * int)) =
    let rec search elapsed robots =
        if elapsed >= 1000 then
            ()
        else
            let positions = robots |> Seq.map (fun robot -> robot.Position) |> Set.ofSeq

            let map =
                Array.init h (fun i -> Array.init w (fun j -> if Set.contains (i, j) positions then '@' else ' '))

            printfn "t = %d" elapsed
            map |> Array.map System.String |> String.concat ("\n") |> printfn "%s"
            printfn "\n\n\n\n\n"

            search (elapsed + 1) (robots |> Seq.map (simulateRobotMovement 1 w h))

    search 0 robots

    //You can see something like 
    //ASCII art in this cycle.
    // t = 81, 81 + w, 81 + 2w, 81 + 3w, ...
    // t = 30, 30 + h, 30 + 2h, 30 + 3h, ...    

    //Search within a suitable range.
    [ 0..1000 ]
    |> List.choose (fun p -> 
        // 81 + p * w = 30 + q * h
        let numQ = (81 - 30) + p * w
        if 
            numQ % h = 0 
        then 
            Some(81 + p * w)
        else 
            None)
    |> List.head



let parse (input: string) = 
    input.Trim().Replace("\r\n", "\n").Split("\n")
    |> Array.map (fun line ->
        try
            let parts = line.Split(" ")
            if parts.Length <> 2 then
                failwithf $"Invalid line format. Expected 2 parts, got {parts.Length}: %s{line}"
                
            let position, velocity = parts[0], parts[1]
            
            if not (position.StartsWith("p=")) then
                failwithf $"Invalid position format. Expected 'p=', got: %s{position}"
            if not (velocity.StartsWith("v=")) then
                failwithf $"Invalid velocity format. Expected 'v=', got: %s{velocity}"
                
            let pos = position.Replace("p=", "").Split(",")
            let vel = velocity.Replace("v=", "").Split(",")
            
            if pos.Length <> 2 then
                failwithf $"Invalid position coordinates. Expected 2 values, got {pos.Length}: %s{position}"
            if vel.Length <> 2 then
                failwithf $"Invalid velocity coordinates. Expected 2 values, got {vel.Length}: %s{velocity}"
                
            let parseCoordinate (value: string) label =
                match System.Int32.TryParse(value) with
                | true, num -> num
                | false, _ -> failwithf $"Invalid {label} coordinate. Expected integer, got: %s{value}"
                
            {
                Position = parseCoordinate pos.[0] "position X", parseCoordinate pos.[1] "position Y"
                Velocity = parseCoordinate vel.[0] "velocity X", parseCoordinate vel.[1] "velocity Y"
            }
        with e ->
            printfn $"\n\nError parsing line:\n%s{line}"
            raise e
    )

module Example =
    let input =
        "p=0,4 v=3,-3
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
p=9,5 v=-3,-3"

    [<Test>]
    let ``Part 1 should calculate correct safety factor`` () =
        (parse input, 7, 11) |> part1 |> should equal 12


[<EntryPoint>]
let main _ = 
    let input = stdin.ReadToEnd().TrimEnd() 
    let robots = parse input

    let stopwatch = Stopwatch.StartNew()

    (robots, 101, 103) |> part1 |> printfn "Part 1: %d"
    (robots, 101, 103) |> part2 |> printfn "Part 2: %d"

    stopwatch.Stop()
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"   

    0 // return an integer exit code
  