module day15

open System.Diagnostics
open NUnit.Framework
open FsUnit

type Cell =
    | Robot
    | Box
    | Wall
    | Empty

type Direction =
    | Up
    | Left
    | Down
    | Right

let swap (i, j) (a: 'T[]) =
    let x, y = a[i], a[j]
    a |> Array.updateAt i y |> Array.updateAt j x

let transpose (a: 'T[][]) =
    let h, w = a.Length, a[0].Length
    Array.init w (fun i -> Array.init h (fun j -> a[j][i]))

let findRobot (map: Cell[][]) =
    List.allPairs [ 0 .. (map.Length - 1) ] [ 0 .. (map[0].Length - 1) ]
    |> List.find (fun (i, j) -> map[i][j] = Robot)

let rec pushLeft (i, j) (map: Cell[][]) =
    assert (map[i][j] = Box)

    match map[i][j - 1] with
    | Wall -> None
    | Empty ->
        // #..OOO. -> # .O.OO.
        let newRow = map[i] |> swap (j - 1, j)
        map |> Array.updateAt i newRow |> Some
    | Box ->
        pushLeft (i, j - 1) map
        |> Option.map (fun map ->
            assert (map[i][j - 1] = Empty)
            // delegate
            pushLeft (i, j) map |> Option.get)
    | c -> failwithf $"%A{c} !?"

let rec moveLeft (map: Cell[][]) =
    let ri, rj = findRobot map

    match map[ri][rj - 1] with
    | Wall -> map
    | Empty ->
        let newRow = map[ri] |> swap (rj - 1, rj)
        map |> Array.updateAt ri newRow
    | Box ->
        map
        |> pushLeft (ri, rj - 1)
        |> Option.map (fun map ->
            assert (map[ri][rj - 1] = Empty)
            // delegate
            moveLeft map)
        |> Option.defaultWith (fun () -> map)
    | Robot -> failwith "!?"

let moveRight (map: Cell[][]) =
    let reverse (a: 'T[][]) = Array.map Array.rev a

    map |> reverse |> moveLeft |> reverse

let moveUp (map: Cell[][]) =
    map |> transpose |> moveLeft |> transpose

let moveDown (map: Cell[][]) =
    map |> transpose |> moveRight |> transpose

let part1 ((map, moves): Cell[][] * Direction seq) =
    let map =
        (map, moves)
        ||> Seq.fold (fun map direction ->
            let mv =
                match direction with
                | Up -> moveUp
                | Left -> moveLeft
                | Down -> moveDown
                | Right -> moveRight

            mv map)

    List.allPairs [ 0 .. (map.Length - 1) ] [ 0 .. (map[0].Length - 1) ]
    |> List.sumBy (fun (i, j) -> if map[i][j] = Box then 100 * i + j else 0)


type ScaledCell =
    | ScaledRobot
    | BoxL
    | BoxR
    | ScaledWall
    | ScaledEmpty

let downgrade =
    function
    | ScaledRobot -> Robot
    | BoxL
    | BoxR -> Box
    | ScaledWall -> Wall
    | ScaledEmpty -> Empty



let findRobotScaled (map: ScaledCell[][]) =
    map |> Array.map (fun row -> row |> Array.map downgrade) |> findRobot


let rec pushLeftScaled (i, j) (map: ScaledCell[][]) =
    assert (map[i][j] = BoxR)
    assert (map[i][j - 1] = BoxL)

    match map[i][j - 2] with
    | ScaledWall -> None
    | ScaledEmpty ->
        // #..[].. -> #.[]...
        let newRow = map[i] |> swap (j - 2, j - 1) |> swap (j - 1, j)
        map |> Array.updateAt i newRow |> Some
    | BoxR ->
        pushLeftScaled (i, j - 2) map
        |> Option.map (fun map ->
            assert (map[i][j - 2] = ScaledEmpty)
            // delegate
            pushLeftScaled (i, j) map |> Option.get)
    | c -> failwithf $"%A{c} !?"

let rec pushUpScaled (i, j) (map: ScaledCell[][]) =
    match map[i][j] with
    | BoxL ->
        assert (map[i][j + 1] = BoxR)

        match map[i - 1][j], map[i - 1][j + 1] with
        | ScaledWall, _
        | _, ScaledWall -> None
        | ScaledEmpty, ScaledEmpty ->
            let newRow1 = map[i - 1] |> Array.updateAt j BoxL |> Array.updateAt (j + 1) BoxR
            let newRow2 = map[i] |> Array.updateAt j ScaledEmpty |> Array.updateAt (j + 1) ScaledEmpty
            map |> Array.updateAt (i - 1) newRow1 |> Array.updateAt i newRow2 |> Some
        // []
        | BoxL, BoxR
        // ].
        | BoxR, ScaledEmpty ->
            pushUpScaled (i - 1, j) map
            |> Option.map (fun map ->
                assert (map[i - 1][j] = ScaledEmpty)
                assert (map[i - 1][j + 1] = ScaledEmpty)
                // delegate
                pushUpScaled (i, j) map |> Option.get)
        // .[
        | ScaledEmpty, BoxL ->
            pushUpScaled (i - 1, j + 1) map
            |> Option.map (fun map ->
                assert (map[i - 1][j] = ScaledEmpty)
                assert (map[i - 1][j + 1] = ScaledEmpty)
                // delegate
                pushUpScaled (i, j) map |> Option.get)
        // ][
        | BoxR, BoxL ->
            pushUpScaled (i - 1, j) map
            |> Option.bind (fun map ->
                assert (map[i - 1][j - 1] = ScaledEmpty)
                assert (map[i - 1][j] = ScaledEmpty)
                pushUpScaled (i - 1, j + 1) map)
            |> Option.map (fun map ->
                assert (map[i - 1][j] = ScaledEmpty)
                assert (map[i - 1][j + 1] = ScaledEmpty)
                // delegate
                pushUpScaled (i, j) map |> Option.get)
        | c1, c2 -> failwithf $"%A{c1} %A{c2} !?"
    | BoxR ->
        // delegate
        pushUpScaled (i, j - 1) map
    | c -> failwithf $"%A{c} !?"

let rec moveLeftScaled (map: ScaledCell[][]) =
    let ri, rj = findRobotScaled map

    match map[ri][rj - 1] with
    | ScaledWall -> map
    | ScaledEmpty ->
        // #...@. -> #..@..
        let newRow = map[ri] |> swap (rj - 1, rj)
        map |> Array.updateAt ri newRow
    | BoxR ->
        map
        |> pushLeftScaled (ri, rj - 1)
        |> Option.map (fun map ->
            assert (map[ri][rj - 1] = ScaledEmpty)
            // delegate
            moveLeftScaled map)
        |> Option.defaultWith (fun () -> map)
    | c -> failwithf $"%A{c} !?"

let rec moveUpScaled (map: ScaledCell[][]) =
    let ri, rj = findRobotScaled map

    match map[ri - 1][rj] with
    | ScaledWall -> map
    | ScaledEmpty ->
        let newRow1 = map[ri - 1] |> Array.updateAt rj ScaledRobot
        let newRow2 = map[ri] |> Array.updateAt rj ScaledEmpty
        map |> Array.updateAt (ri - 1) newRow1 |> Array.updateAt ri newRow2
    | BoxL
    | BoxR ->
        map
        |> pushUpScaled (ri - 1, rj)
        |> Option.map (fun map ->
            assert (map[ri - 1][rj] = ScaledEmpty)
            // delegate
            moveUpScaled map)
        |> Option.defaultWith (fun () -> map)
    | c -> failwithf $"%A{c} !?"

let moveRightScaled (map: ScaledCell[][]) =
    let reverse (map: ScaledCell[][]) =
        map
        |> Array.map (fun row ->
            row
            |> Array.rev
            |> Array.map (function
                | BoxL -> BoxR
                | BoxR -> BoxL
                | c -> c))

    map |> reverse |> moveLeftScaled |> reverse

let moveDownScaled (map: ScaledCell[][]) = map |> Array.rev |> moveUpScaled |> Array.rev

let scaleUp (map: Cell[][]) =
    map
    |> Array.map (fun row ->
        row
        |> Array.collect (function
            | Cell.Robot -> [| ScaledRobot; ScaledEmpty |]
            | Box -> [| BoxL; BoxR |]
            | Cell.Wall -> [| ScaledWall; ScaledWall |]
            | Cell.Empty -> [| ScaledEmpty; ScaledEmpty |]))

let part2 ((map, moves): Cell[][] * Direction seq) =
    let map =
        (scaleUp map, moves)
        ||> Seq.fold (fun map dir ->
            let mv =
                match dir with
                | Up -> moveUpScaled
                | Left -> moveLeftScaled
                | Down -> moveDownScaled
                | Right -> moveRightScaled

            mv map)

    List.allPairs [ 0 .. (map.Length - 1) ] [ 0 .. (map[0].Length - 1) ]
    |> List.sumBy (fun (i, j) -> if map[i][j] = BoxL then i * 100 + j else 0)


let parseMap (input: string) =
    input.Split('\n', System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun row ->
        row.ToCharArray()
        |> Array.map (function
            | '@' -> Robot
            | 'O' -> Box
            | '#' -> Wall
            | '.' -> Empty
            | c -> failwith $"{c} !?"))

let parse (input: string) =
    let trimmed = input.Replace("\r", "")

    let input =
        trimmed.Split([| "\n\n" |], System.StringSplitOptions.RemoveEmptyEntries)

    let map, moves = input[0], input[1]

    let map = parseMap map

    let moves =
        moves.Split("\n")
        |> Array.collect (fun moves ->
            moves.ToCharArray()
            |> Array.map (function
                | '^' -> Up
                | '<' -> Left
                | 'v' -> Down
                | '>' -> Right
                | c -> failwith $"{c} !?"))

    map, moves


module Day15TestsSuites =
    // Test input data
    let exampleInput = "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<"

    let exampleInputLarge = "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"

    // Helper function for cell array printing
    let cellArrayPrinter (map: Cell[][]) =
        map 
        |> Array.map (fun row ->
            row 
            |> Array.map (function
                | Robot -> "@"
                | Box -> "O"
                | Wall -> "#"
                | Empty -> ".")
            |> String.concat "")
        |> String.concat "\n"

    let scaledCellArrayPrinter (map: ScaledCell[][]) =
        map 
        |> Array.map (fun row ->
            row 
            |> Array.map (function
                | ScaledRobot -> "@"
                | BoxL -> "["
                | BoxR -> "]"
                | ScaledWall -> "#"
                | ScaledEmpty -> ".")
            |> String.concat "")
        |> String.concat "\n"

    [<TestFixture>]
    type Day15Tests() =
        
        [<Test>]
        member _.``Basic movement - move left``() =
            let input = "#..OO@."
            let expected = "#.OO@.."
            let map = parseMap input
            let moved = moveLeft map
            moved |> cellArrayPrinter |> should equal (parseMap expected |> cellArrayPrinter)

        [<Test>]
        member _.``Basic movement - move right``() =
            let input = "#.@OO.."
            let expected = "#..@OO."
            let map = parseMap input
            let moved = moveRight map
            moved |> cellArrayPrinter |> should equal (parseMap expected |> cellArrayPrinter)

        [<Test>]
        member _.``Basic movement - move up``() =
            let input = "######\n#....#\n#.OO@#\n######"
            let expected = "######\n#...@#\n#.OO.#\n######"
            let map = parseMap input
            let moved = moveUp map
            moved |> cellArrayPrinter |> should equal (parseMap expected |> cellArrayPrinter)

        [<Test>]
        member _.``Basic movement - move down``() =
            let input = "######\n#.@OO#\n#....#\n######"
            let expected = "######\n#..OO#\n#.@..#\n######"
            let map = parseMap input
            let moved = moveDown map
            moved |> cellArrayPrinter |> should equal (parseMap expected |> cellArrayPrinter)

        // Scaled movement tests
        [<Test>]
        member _.``Scaled movement - move left``() =
            let input = "#..OO@."
            let expected = "##...[][]@...."
            let map = parseMap input |> scaleUp
            let moved = moveLeftScaled map
            moved |> scaledCellArrayPrinter |> should equal expected

        [<Test>]
        member _.``Scaled movement - move right``() =
            let input = "#.@OO.."
            let expected = "##...@[][]...."
            let map = parseMap input |> scaleUp
            let moved = moveRightScaled map
            moved |> scaledCellArrayPrinter |> should equal expected

        [<Test>]
        member _.``Scaled movement - move left v2``() =
            let input = "######\n#....#\n#.OO@#\n######"
            let expected = "############\n##........##\n##.[][]@..##\n############"
            let map = parseMap input |> scaleUp
            let moved = moveLeftScaled map
            moved |> scaledCellArrayPrinter |> should equal expected

        [<Test>]
        member _.``Scaled movement - move right v2``() =
            let input = "#######\n#.....#\n#.OO@.#\n#######"
            let expected = "##############\n##..........##\n##..[][].@..##\n##############"
            let map = parseMap input |> scaleUp
            let moved = moveRightScaled map
            moved |> scaledCellArrayPrinter |> should equal expected

        [<Test>]
        member _.``Scaled movement - move up``() =
            let input = "######\n#....#\n#.OO@#\n######"
            let expected = "######\n#...@#\n#.OO.#\n######"
            let map = parseMap input |> scaleUp
            let moved = moveUpScaled map
            moved |> scaledCellArrayPrinter |> should equal (parseMap expected |> scaleUp |> scaledCellArrayPrinter)

        [<Test>]
        member _.``Scaled movement - move down``() =
            let input = "######\n#.@OO#\n#....#\n######"
            let expected = "######\n#..OO#\n#.@..#\n######"
            let map = parseMap input |> scaleUp
            let moved = moveDownScaled map
            moved |> scaledCellArrayPrinter |> should equal (parseMap expected |> scaleUp |> scaledCellArrayPrinter)

        // Game logic tests
        [<Test>]
        member _.``Part 1 example``() =
            let map, moves = parse exampleInput
            part1 (map, moves) |> should equal 2028

        [<Test>]
        member _.``Part 2 example``() =
            let map, moves = parse exampleInputLarge
            part2 (map, moves) |> should equal 9021


[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let map, moves = parse input

    let stopwatch = Stopwatch.StartNew()

    (map, moves) |> part1 |> printfn "Part 1: %d"
    (map, moves) |> part2 |> printfn "Part 2: %d"

    stopwatch.Stop()
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"

    0
