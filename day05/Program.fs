module Day05

open NUnit.Framework
open FsUnit

let splitUpdates (rules: (int * int) seq) (updates: int seq seq) =
    let pages = rules |> Seq.collect (fun (p, q) -> [ p; q ]) |> Seq.distinct

    Seq.allPairs pages pages
    |> Seq.iter (fun (p, q) ->
        //All pairs appear in rules 
        (p = q || Seq.contains (p, q) rules || Seq.contains (q, p) rules) |> should be True)
        
    let following =
        (Map.empty, rules)
        ||> Seq.fold (fun acc (p, q) ->
            let v = acc |> Map.tryFind p |> Option.defaultValue Set.empty
            Map.add p (Set.add q v) acc)

    let correct, incorrect =
        updates
        |> List.ofSeq
        |> List.map List.ofSeq
        |> List.partition (fun updates ->
            updates
            |> List.indexed
            |> List.forall (fun (i, p) ->
                List.skip (i + 1) updates
                |> List.forall (fun q ->
                    // Check if q should follow p
                    let ``p,q`` =
                        Map.tryFind p following
                        |> Option.map (fun v -> Set.contains q v)
                        |> Option.defaultValue true
                
                    // Check if p should not follow q
                    let ``q,p`` =
                        Map.tryFind q following
                        |> Option.map (fun v -> Set.contains p v |> not)
                        |> Option.defaultValue true

                    ``p,q`` && ``q,p``))) // Both conditions must be true

    (following, (correct, incorrect))

let part1 (rules: (int * int) seq) (updates: int seq seq) =
    let _, (correctUpdates, _) = splitUpdates rules updates

    correctUpdates |> List.sumBy (fun updates -> updates[updates.Length / 2])

let part2 (rules: (int * int) seq) (updates: int seq seq) =
    let following, (_, incorrectUpdates) = splitUpdates rules updates

    incorrectUpdates
    |> List.sumBy (fun updates ->
        let sorted =
            updates
            |> List.sortWith (fun p q ->
                following
                |> Map.tryFind p
                |> Option.map (fun v -> if Set.contains q v then -1 else 1)
                |> Option.defaultValue 1)

        sorted[sorted.Length / 2])

let parse (input: string) =
    let blocks =
        input
            .Replace("\r\n", "\n")
            .Trim()
            .Split("\n\n", System.StringSplitOptions.RemoveEmptyEntries)

    let first = if blocks.Length > 0 then blocks.[0] else ""
    let second = if blocks.Length > 1 then blocks.[1] else ""

    let rules =
        first.Split([| '\n' |], System.StringSplitOptions.RemoveEmptyEntries)
        |> Seq.choose (fun line ->
            let parts =
                line.Trim().Split([| '|' |], System.StringSplitOptions.RemoveEmptyEntries)

            if parts.Length = 2 then
                Some(int parts.[0], int parts.[1])
            else
                None)

    let updates =
        second.Split([| '\n' |], System.StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun line ->
            line.Split([| ',' |], System.StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map (fun x -> int (x.Trim())))

    (rules, updates)

module Example =
    let input =
        "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"

    [<Test>]
    let ``testPart1`` () =
        parse input ||> part1 |> should equal 143

    [<Test>]
    let ``testPart2`` () =
        parse input ||> part2 |> should equal 123

open System.Diagnostics
[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let rules, updates = parse input

    let stopwatch = Stopwatch.StartNew()

    let part1Result = (rules, updates) ||> part1
    printfn "Part 1: %d" part1Result

    let part2Result = (rules, updates) ||> part2
    printfn "Part 2: %d" part2Result

    stopwatch.Stop()
    printfn "Elapsed time: %A" stopwatch.Elapsed

    0
