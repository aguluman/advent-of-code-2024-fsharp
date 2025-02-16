open System.Text.RegularExpressions
open Xunit
open FsUnit.Xunit

type Machine =
    { ButtonA: Button
      ButtonB: Button
      Prize: Prize }

and Button = { AddX: int; AddY: int }
and Prize = { X: int; Y: int }

let tryMin list' =
    if List.isEmpty list' then None else Some(List.min list')

let part1 (machines: Machine seq) =
    machines
    |> Seq.sumBy
        (fun
            { ButtonA = buttonA
              ButtonB = buttonB
              Prize = prize } ->
            List.allPairs [ 0..100 ] [ 0..100 ]
            |> List.filter (fun (i, j) ->
                let x = buttonA.AddX * i + buttonB.AddX * j
                let y = buttonA.AddY * i + buttonB.AddY * j
                x = prize.X && y = prize.Y)
            |> List.map (fun (i, j) -> i * 3 + j)
            |> tryMin
            |> Option.defaultValue 0)


let part2 (machines: Machine seq) =
    machines
    |> Seq.sumBy
        (fun
            { ButtonA = { AddX = ax; AddY = ay }
              ButtonB = { AddX = bx; AddY = by }
              Prize = { X = px; Y = py } } ->
            let px, py = int64 px + 10000000000000L, int64 py + 10000000000000L

            let det = int64 (ax * by - bx * ay)

            if det = 0L then
                0L
            else
                let numI = int64 by * px - int64 bx * py
                let numJ = int64 -ay * px + int64 ax * py

                if numI % det = 0L && numJ % det = 0L then
                    let i = numI / det
                    let j = numJ / det
                    i * 3L + j
                else
                    0L)


let parse (input: string) =
    input.Split("\n\n", System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun section ->
        let lines =
            section.Split('\n', System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun s -> s.Trim())
            |> Array.filter (not << System.String.IsNullOrWhiteSpace)

        if lines.Length <> 3 then
            printfn $"Debug: Found %d{lines.Length} lines in section: %s{section}"
            failwithf $"Invalid section format: Expected exactly 3 lines, got %d{lines.Length}"

        let parseButton (line: string) expectedButton =
            let pattern = $@"^{expectedButton}: X\+(\d+), Y\+(\d+)$"
            let m = Regex.Match(line, pattern)

            if not m.Success then
                failwithf $"Invalid button format for %s{expectedButton}: %s{line}"

            { AddX = int m.Groups[1].Value
              AddY = int m.Groups[2].Value }

        let parsePrize (line: string) =
            let pattern = @"^Prize: X=(\d+), Y=(\d+)$"
            let m = Regex.Match(line, pattern)

            if not m.Success then
                failwithf $"Invalid prize format: %s{line}"

            { X = int m.Groups[1].Value
              Y = int m.Groups[2].Value }

        try
            let buttonA = parseButton lines[0] "Button A"
            let buttonB = parseButton lines[1] "Button B"
            let prize = parsePrize lines[2]

            { ButtonA = buttonA
              ButtonB = buttonB
              Prize = prize }

        with e ->
            printfn $"Error parsing section:\n%s{section}"
            raise e)



module Example =
    let input = """Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"""

    [<Fact>]
    let testPart1 () =
        parse input |> part1 |> should equal 480


open System.Diagnostics

[<EntryPoint>]
let main _ =
    let input =
        stdin.ReadToEnd()
        |> fun s -> s.Trim()
        |> fun s -> s.Replace("\r\n", "\n") // Normalize line endings

    let machines = parse input

    let stopwatch = Stopwatch.StartNew()

    machines |> part1 |> printfn "Part 1: %d"
    machines |> part2 |> printfn "Part 2: %d"

    stopwatch.Stop()
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"

    0
