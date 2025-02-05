open Xunit
open FsUnit.Xunit

type Block =
    | Free
    | Occupied of int //id

let checksum (disk: Block[]) =
    disk
    |> Array.indexed
    |> Array.sumBy (fun (i, b) ->
        match b with
        | Free -> 0L
        | Occupied id -> (int64 i) * (int64 id))


let part1 (disk: Block[]) =
    let rec compact leftPos rightPos (disk: Block[]) =
        if leftPos >= rightPos then
            disk
        else
            match (disk[leftPos], disk[rightPos]) with
            | (Free, Free) -> compact leftPos (rightPos - 1) disk
            | (Free, Occupied id) ->
                //swap disk[leftPos], disk[rightPos]
                let newDisk = disk |> Array.updateAt leftPos (Occupied id) |> Array.updateAt  rightPos Free
                compact (leftPos + 1) (rightPos - 1) newDisk
            | (Occupied _, Free) -> compact (leftPos + 1) (rightPos - 1) disk
            | (Occupied _, Occupied _) -> compact (leftPos + 1) rightPos disk

    disk |> compact 0 (disk.Length - 1) |> checksum

let part2 (disk: Block[]) =
    let rec compact rightPos (disk: Block[]) =
        if rightPos <= 0 then 
            disk
        else 
            match disk[rightPos] with
            | Free -> compact (rightPos - 1) disk
            | Occupied id ->
                // Find length of contiguous occupied blocks
                let length = 
                    let mutable count = 0
                    while count <= rightPos && disk[rightPos - count] = Occupied id do
                        count <- count + 1
                    count
                
                let newR = rightPos - length

                // (Find Free Space) Find first position that can fit length Free blocks
                let rec findFreePos leftPos =
                    if leftPos > rightPos then 
                        None
                    else 
                        let mutable isFree = true
                        for i in 0..length - 1 do
                            if leftPos + i >= disk.Length || disk[leftPos + i] <> Free then
                                isFree <- false
                        if isFree then 
                            Some leftPos
                        else 
                            findFreePos (leftPos + 1)

                //Block Movement
                match findFreePos 0 with
                | None -> 
                    compact newR disk
                | Some leftPos ->
                    // Swap blocks to new position
                    for d in 0..length - 1 do
                        disk[leftPos + d] <- Occupied id
                        disk[newR + 1 + d] <- Free
                    compact newR disk

    let diskCopy = Array.copy disk
    checksum (compact (diskCopy.Length - 1) diskCopy)

let parse (input: string) =
    input.ToCharArray()
    |> Array.map (fun c -> int c - int '0')
    |> Array.indexed
    |> Array.collect (fun (i, d) -> 
        let b = if i % 2 = 0 then Occupied(i / 2) else Free
        Array.replicate d b)

module Example = 
    let input = "2333133121414131402"

    [<Fact>]
    let testPart1 () = 
        parse input |> part1 |> should equal 1928L

    [<Fact>]
    let testPart2 () = 
        parse input |> part2 |> should equal 2858L
        
open System.Diagnostics

[<EntryPoint>]
let main _ = 
    let input = stdin.ReadToEnd().TrimEnd()
    let disk = parse input

    let stopwatch = Stopwatch.StartNew()

    disk |> part1 |> printfn "Part 1: %d"
    disk |> part2 |> printfn "Part 2: %d"

    stopwatch.Stop()
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"
    
    0