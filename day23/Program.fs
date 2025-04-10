module day23

open System.Collections.Generic
open System.Diagnostics
open NUnit.Framework
open FsUnit


open System
open System.IO
open System.Collections.Generic

// Check if three nodes form a connected triple
let allThree (a: string) (b: string) (c: string) (connections: Map<string, string list>) =
    let hasConnection src dest =
        match connections.TryFind src with
        | Some neighbors -> List.contains dest neighbors
        | None -> false
            
    hasConnection b a && hasConnection a c && hasConnection b c

// Read and parse input file
let parseInput filename =
    File.ReadAllLines(filename)
    |> Array.fold (fun (connections: Map<string, string list>) line ->
        let parts = line.Split('-')
        let src, dest = parts.[0], parts.[1]
        
        // Add bidirectional connections
        let connections' = 
            match connections.TryFind src with
            | Some neighbors -> connections.Add(src, dest :: neighbors)
            | None -> connections.Add(src, [dest])
                
        match connections'.TryFind dest with
        | Some neighbors -> connections'.Add(dest, src :: neighbors)
        | None -> connections'.Add(dest, [src])
    ) Map.empty

[<EntryPoint>]
let main argv =
    let connections = parseInput @"c:\Users\chukw\Downloads\input.txt"
    
    printfn $"Input length: %d{connections.Count}"



    let stopwatch = Stopwatch()
    stopwatch.Start()

    // Part 1: Count connected triples with at least one node starting with 't'
    let mutable groups = Set.empty
    let mutable ans = 0
    
    for KeyValue(s, neighbors) in connections do
        for s1 in neighbors do
            match connections.TryFind s1 with
            | Some s1Neighbors ->
                for s2 in s1Neighbors do
                    // Create a sorted tuple to avoid counting the same group multiple times
                    let sortedNodes = [s; s1; s2] |> List.sort
                    let key = (sortedNodes.[0], sortedNodes.[1], sortedNodes.[2])
                    
                    if allThree s s1 s2 connections && not (groups.Contains key) then
                        groups <- groups.Add key
                        if s.[0] = 't' || s1.[0] = 't' || s2.[0] = 't' then
                            ans <- ans + 1
            | None -> ()
    
    printfn "Part 1: %d" ans
    
    // Part 2: Find the largest clique
    let mutable maxClique = []
    let mutable maxCliqueSize = 0
    
    for KeyValue(s, _) in connections do
        let mutable nodes = [s]
        
        let rec addConnectedNodes () =
            let mutable added = false
            
            for KeyValue(sPotential, vNeighbors) in connections do
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
            
            if added then addConnectedNodes()
        
        addConnectedNodes()
        
        if List.length nodes > maxCliqueSize then
            maxClique <- nodes
            maxCliqueSize <- List.length nodes
    
    printfn "Part 2: %d" maxCliqueSize
    printfn "Clique nodes: %s" (String.Join(", ", maxClique |> List.sort))
    
    stopwatch.Stop()

    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"

    0
