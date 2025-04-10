/// <summary>
/// Day 23: Triples and Cliques - Graph Connection Analysis
/// </summary>
/// <description>
/// Solves Advent of Code Day 23 challenge about analyzing connections in a graph.
/// The module identifies connected triples and finds the largest complete subgraph (clique).
/// </description>
///
/// <remarks>
/// Problem details:
/// - Input: A list of bidirectional connections between nodes
/// - Part 1: Count connected triples where at least one node starts with 't'
/// - Part 2: Find the size of the largest clique in the graph
///
/// See: <see href="https://adventofcode.com/2024/day/23">Advent of Code 2024, Day 23</see>
/// </remarks>
module day23

open System
open System.Diagnostics
open NUnit.Framework
open FsUnit

/// <summary>
/// Checks if three nodes form a connected triple (a complete subgraph of size 3).
/// </summary>
/// <param name="a">First node</param>
/// <param name="b">Second node</param>
/// <param name="c">Third node</param>
/// <param name="connections">Map of nodes to their connected neighbors</param>
/// <returns>True if the three nodes form a connected triple, false otherwise</returns>
let allThree (a: string) (b: string) (c: string) (connections: Map<string, string list>) =
    let hasConnection src dest =
        match connections.TryFind src with
        | Some neighbors -> List.contains dest neighbors
        | None -> false

    hasConnection b a && hasConnection a c && hasConnection b c


/// <summary>
/// Counts connected triples where at least one node starts with 't'.
/// </summary>
/// <param name="connections">Map of nodes to their connected neighbors</param>
/// <returns>Count of connected triples with at least one 't' node</returns>
let part1 (connections: Map<string, string list>) =
    let mutable groups = Set.empty
    let mutable ans = 0

    for KeyValue(s, neighbors) in connections do
        for s1 in neighbors do
            match connections.TryFind s1 with
            | Some s1Neighbors ->
                for s2 in s1Neighbors do
                    // Create a sorted tuple to avoid counting the same group multiple times
                    let sortedNodes = [ s; s1; s2 ] |> List.sort
                    let key = (sortedNodes.[0], sortedNodes.[1], sortedNodes.[2])

                    if allThree s s1 s2 connections && not (groups.Contains key) then
                        groups <- groups.Add key

                        if s.[0] = 't' || s1.[0] = 't' || s2.[0] = 't' then
                            ans <- ans + 1
            | None -> ()

    ans



/// <summary>
/// Finds the largest clique (complete subgraph) in the graph.
/// </summary>
/// <param name="connections">Map of nodes to their connected neighbors</param>
/// <returns>Comma-separated string of nodes in the largest clique, sorted alphabetically</returns>
let part2 (connections: Map<string, string list>) =
    let mutable maxClique = []
    let mutable maxCliqueSize = 0

    for KeyValue(s, _) in connections do
        let mutable nodes = [ s ]

        let rec addConnectedNodes () =
            let mutable added = false

            for KeyValue(sPotential, _) in connections do
                if not (List.contains sPotential nodes) then
                    // Check if sPotential is connected to all nodes in the current clique
                    let connectedToAll =
                        nodes
                        |> List.forall (fun n ->
                            match connections.TryFind sPotential with
                            | Some neighbors -> List.contains n neighbors
                            | None -> false)

                    if connectedToAll then
                        nodes <- sPotential :: nodes
                        added <- true

            if added then
                addConnectedNodes ()

        addConnectedNodes ()

        if List.length nodes > maxCliqueSize then
            maxClique <- nodes
            maxCliqueSize <- List.length nodes

    // Return comma-separated string of clique nodes
    maxClique |> List.sort |> String.concat ","


/// <summary>
/// Parses the input into a map of connections.
/// </summary>
/// <param name="input">String containing the list of connections</param>
/// <returns>Map of nodes to their connected neighbors</returns>
let parse (input: string) =
    input.Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.fold
        (fun (connections: Map<string, string list>) line ->
            let parts = line.Trim().Split('-')
            let src, dest = parts.[0], parts.[1]

            // Add bidirectional connections
            let connections' =
                match connections.TryFind src with
                | Some neighbors -> connections.Add(src, dest :: neighbors)
                | None -> connections.Add(src, [ dest ])

            match connections'.TryFind dest with
            | Some neighbors -> connections'.Add(dest, src :: neighbors)
            | None -> connections'.Add(dest, [ src ]))
        Map.empty



/// <summary>
/// Example test cases and assertions for the solution
/// </summary>
module Example =
    let input =
        "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn"

    [<Test>]
    let testPart1 () = parse input |> part1 |> should equal 7

    [<Test>]
    let testPart2 () =
        parse input |> part2 |> should equal "co,de,ka,ta"

/// <summary>Main entry point for the program</summary>
[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    printfn $"Input length: %d{input.Length}"


    let stopwatch = Stopwatch()
    stopwatch.Start()

    let connections = parse input
    printfn $"Connections count: %d{connections.Count}"

    let part1Result = part1 connections
    printfn "Part 1: %d" part1Result

    let part2Result = part2 connections
    printfn "Part 2: %s" part2Result

    // Also output the size for compatibility with the original output
    let clique = part2Result.Split(',')
    printfn "Clique size: %d" clique.Length

    stopwatch.Stop()
    printfn "Elapsed time: %.4f seconds" stopwatch.Elapsed.TotalSeconds

    0
