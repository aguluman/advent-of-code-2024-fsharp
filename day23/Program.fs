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
open System.Collections.Generic
open NUnit.Framework
open FsUnit



/// <summary>
/// Efficiently checks if three nodes form a connected triple (triangle) in the graph.
/// </summary>
/// <remarks>
/// <para>The function considers a triple connected if each node connects directly to the other two.</para>
/// </remarks>
/// <param name="a">First node identifier</param>
/// <param name="b">Second node identifier</param>
/// <param name="c">Third node identifier</param>
/// <param name="connections">Adjacency map representing node connections</param>
/// <returns><c>true</c> if nodes form a triangle; otherwise, <c>false</c></returns>
let inline areTriangle a b c (connections: Dictionary<string, HashSet<string>>) =
    connections[a].Contains b
    && connections[b].Contains c
    && connections[c].Contains a



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
let part1 (connections: Dictionary<string, HashSet<string>>) =
    let discovered = HashSet<string>()
    let mutable count = 0

    for KeyValue(node, neighbors) in connections do
        for neighbor in neighbors do
            // Early filtering: Only allow lexically greater nodes to avoid recounts
            if String.Compare(node, neighbor) < 0 then
                for nextNeighbor in connections[neighbor] do
                    // Ensure consistent ordering and uniqueness
                    if
                        String.Compare(neighbor, nextNeighbor) < 0
                        && areTriangle node neighbor nextNeighbor connections
                    then
                        let triple = [| node; neighbor; nextNeighbor |] |> Array.sort
                        let key = String.Join(",", triple)

                        if discovered.Add key && triple |> Array.exists (fun x -> x.StartsWith "t") then
                            count <- count + 1

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
let part2 (connections: Dictionary<string, HashSet<string>>) =
    let nodes = connections.Keys |> Seq.toArray
    let largestClique = ref Array.empty<string>

    /// Efficiently checks if the candidate can be added to the current clique
    let inline canAddToClique clique candidate =
        clique |> Array.forall (fun n -> connections[candidate].Contains n)

        /// Recursive search for clique with aggressive early termination
    let rec growClique (currentClique: string[]) candidates =
        match candidates with
        | [||] ->
            if currentClique.Length > largestClique.Value.Length then
                largestClique.Value <- currentClique
        | _ ->
            // Helper function to process candidates one by one in a tail-recursive manner
            let rec processCandidates index =
                if index >= candidates.Length then
                    () // Done processing all candidates
                else
                    let candidate = candidates[index]
                    
                    // Early termination check
                    if currentClique.Length + (candidates.Length - index) <= largestClique.Value.Length then
                        // Skip remaining candidates as they can't improve our result
                        ()
                    else
                        let nextClique = Array.append currentClique [| candidate |]
                        let nextCandidates =
                            candidates[(index + 1)..] |> Array.filter (canAddToClique nextClique)
                        
                        // Process this branch
                        growClique nextClique nextCandidates
                        
                        // Continue with the next candidate
                        processCandidates (index + 1)
            
            // Start processing from the first candidate
            processCandidates 0

    // Sort nodes to improve clique-finding performance through informed pruning
    let sortedNodes = nodes |> Array.sortByDescending (fun n -> connections[n].Count)

    // Run parallel tasks for distinct starting nodes
    sortedNodes
    |> Array.Parallel.iter (fun node ->
        let candidates =
            connections[node]
            |> Seq.toArray
            |> Array.filter (fun n -> String.CompareOrdinal(n, node) > 0)

        growClique [| node |] candidates)

    // Return the largest clique sorted alphabetically for the password
    largestClique.Value |> Array.sort |> String.concat ","




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


/// Converts from Map<string, string list> to Dictionary<string, HashSet<string>>
let toOptimizedDict (connections: Map<string, string list>) =
    connections
    |> Seq.map (fun kvp -> kvp.Key, HashSet kvp.Value)
    |> dict
    |> Dictionary


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
    let testPart1 () =
        input |> parse |> toOptimizedDict |> part1 |> should equal 7

    [<Test>]
    let testPart2 () =
        input |> parse |> toOptimizedDict |> part2 |> should equal "co,de,ka,ta"



/// <summary>Main entry point for the program</summary>
[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    printfn $"Input length: %d{input.Length}"

    let connections = parse input |> toOptimizedDict

    let stopwatch = Stopwatch()
    stopwatch.Start()

    let result1 = part1 connections
    let result2 = part2 connections

    stopwatch.Stop()

    printfn $"Connections count: %d{connections.Count}"
    printfn $"Part 1: %d{result1}"
    printfn $"Part 2: %s{result2}"
    printfn $"Clique size: %d{result2.Split(',').Length}"
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"

    0
