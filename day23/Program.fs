/// <summary>
/// Day 23: LAN Party - Graph Connection and Clique Analysis
/// </summary>
/// <description>
/// Solves the Advent of Code Day 23 challenges focused on analyzing network connections to locate a LAN party.
/// This module performs two main tasks:
/// <list type="bullet">
///   <item><description><b>Part 1:</b> Count all connected triples (sets of 3 nodes) with at least one node name starting with 't'</description></item>
///   <item><description><b>Part 2:</b> Find the largest clique, which is the biggest subset of nodes where each node connects directly with every other node</description></item>
/// </list>
/// </description>
/// <remarks>
/// <para>Problem details:</para>
/// <list type="bullet">
///   <item><description><b>Input:</b> Network connections represented as bidirectional strings</description></item>
///   <item><description><b>Output (Part 1):</b> Number of triple-connected sets containing at least one node starting with 't'</description></item>
///   <item><description><b>Output (Part 2):</b> Comma-separated node names sorted alphabetically forming the largest clique (password)</description></item>
/// </list>
///
/// <para>Used concepts:</para>
/// <list type="bullet">
///   <item><description>Graph representation as adjacency maps</description></item>
///   <item><description>Search algorithms to identify triangles and larger cliques</description></item>
///   <item><description>String and collection manipulations for efficiency and clarity</description></item>
/// </list>
///
/// See details at: <see href="https://adventofcode.com/2024/day/23">Advent of Code 2024, Day 23</see>
/// </remarks>
module day23

open System
open System.Diagnostics
open NUnit.Framework
open FsUnit

/// <summary>
/// Checks if three nodes form a connected triple (triangle) in the graph.
/// </summary>
/// <remarks>
/// <para>The function considers a triple connected if each node connects directly to the other two.</para>
/// </remarks>
/// <param name="a">First node identifier</param>
/// <param name="b">Second node identifier</param>
/// <param name="c">Third node identifier</param>
/// <param name="connections">Adjacency map representing node connections</param>
/// <returns><c>true</c> if nodes form a triangle; otherwise, <c>false</c></returns>
let allThree (a: string) (b: string) (c: string) (connections: Map<string, string list>) =
    let hasConnection src dest =
        match connections.TryFind src with
        | Some neighbors -> List.contains dest neighbors
        | None -> false

    hasConnection a b && hasConnection b c && hasConnection c a

/// <summary>
/// Counts all sets of three interconnected nodes (triangles) in the network,
/// ensuring that at least one node name starts with 't'.
/// </summary>
/// <remarks>
/// <para>Algorithm:</para>
/// <list type="number">
///   <item><description>Iterate through each node and its neighbors</description></item>
///   <item><description>For each pair of neighbors, verify if they form a connected triple</description></item>
///   <item><description>Ensure no triple is counted more than once by using a sorted tuple set</description></item>
///   <item><description>Increment count only if at least one node in the set begins with 't'</description></item>
/// </list>
/// </remarks>
/// <param name="connections">Adjacency map representing node connections</param>
/// <returns>The total count of qualifying connected triples</returns>
let part1 (connections: Map<string, string list>) =
    let mutable discoveredTriples = Set.empty
    let mutable count = 0

    for KeyValue(node, neighbors) in connections do
        for neighbor1 in neighbors do
            match connections.TryFind neighbor1 with
            | Some neighborsOfNeighbor1 ->
                for neighbor2 in neighborsOfNeighbor1 do
                    let sortedTriple = [ node; neighbor1; neighbor2 ] |> List.sort
                    let key = (sortedTriple[0], sortedTriple[1], sortedTriple[2])

                    if
                        allThree node neighbor1 neighbor2 connections
                        && not (discoveredTriples.Contains key)
                    then
                        discoveredTriples <- discoveredTriples.Add key

                        if [ node; neighbor1; neighbor2 ] |> List.exists (fun name -> name.StartsWith "t") then
                            count <- count + 1
            | None -> ()

    count

/// <summary>
/// Finds the largest clique (fully connected group) in the network.
/// </summary>
/// <remarks>
/// <para>The clique identification involves the following steps:</para>
/// <list type="number">
///   <item><description>Begin with each node and iteratively attempt to add other nodes that connect to every node in the existing group.</description></item>
///   <item><description>Continue recursively until no further nodes can be added.</description></item>
///   <item><description>Keep track of the largest clique found throughout the process.</description></item>
///   <item><description>Return this largest clique sorted alphabetically as the final password.</description></item>
/// </list>
/// </remarks>
/// <param name="connections">Adjacency map representing node connections</param>
/// <returns>
/// A comma-separated, alphabetically sorted string of the nodes constituting the largest clique, serving as the LAN party password.
/// </returns>
let part2 (connections: Map<string, string list>) =
    let mutable largestClique = []
    let mutable largestCliqueSize = 0

    for KeyValue(node, _) in connections do
        let mutable currentClique = [ node ]

        let rec extendClique () =
            let mutable expanded = false

            for KeyValue(candidate, _) in connections do
                if not (currentClique |> List.contains candidate) then
                    let connectsToAll =
                        currentClique
                        |> List.forall (fun cliqueMember ->
                            match connections.TryFind candidate with
                            | Some neighbors -> neighbors |> List.contains cliqueMember
                            | None -> false)

                    if connectsToAll then
                        currentClique <- candidate :: currentClique
                        expanded <- true

            if expanded then
                extendClique ()

        extendClique ()

        if currentClique.Length > largestCliqueSize then
            largestClique <- currentClique
            largestCliqueSize <- currentClique.Length

    largestClique |> List.sort |> String.concat ","


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
            let src, dest = parts[0], parts[1]

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
    printfn $"Part 1: %d{part1Result}"

    let part2Result = part2 connections
    printfn $"Part 2: %s{part2Result}"

    // Also output the size for compatibility with the original output
    let clique = part2Result.Split(',')
    printfn $"Clique size: %d{clique.Length}"

    stopwatch.Stop()
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"

    0
