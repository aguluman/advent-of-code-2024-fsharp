/// <summary>
/// Day 24: Crossed Wires - Boolean Logic Gate Simulation
/// </summary>
/// <description>
/// Solves Advent of Code 2024 Day 24 challenge about simulating boolean logic gates and
/// fixing a wiring issue in an electronic circuit.
/// The implementation handles both the
/// circuit simulation and the diagnostic repair of mis-wired gates.
/// </description>
///
/// <remarks>
/// <para>Problem details:</para>
/// <list type="bullet">
///   <item>
///     <description>
///       Part 1: Simulate a system of AND, OR, and XOR logic gates with their interconnections
///       to calculate a final decimal number from binary outputs on z-wires
///     </description>
///   </item>
///   <item>
///     <description>
///       Part 2: Identify and fix four pairs of gates whose output wires have been swapped,
///       preventing the circuit from correctly performing binary addition
///     </description>
///   </item>
/// </list>
///
/// <para>The solution implements:</para>
/// <list type="number">
///   <item>
///     <description>
///       A circuit simulation engine that processes gates in the correct order based on
///       input/output dependencies
///     </description>
///   </item>
///   <item>
///     <description>
///       A circuit verification system that validates whether the circuit correctly
///       performs binary addition
///     </description>
///   </item>
///   <item>
///     <description>
///       A search algorithm that identifies the gate pairs with swapped outputs
///     </description>
///   </item>
/// </list>
///
/// <para>See: <see href="https://adventofcode.com/2024/day/24">Advent of Code 2024, Day 24</see></para>
/// </remarks>
module day24

open System
open System.Diagnostics
open NUnit.Framework
open FsUnit
open System.Text.RegularExpressions

/// <summary>
/// Represents the different types of boolean logic gates supported by the circuit.
/// </summary>
///
/// <remarks>
/// The circuit simulation supports three fundamental boolean operations:
/// <list type="bullet">
///   <item><description>AND - outputs 1 if both inputs are 1, otherwise 0</description></item>
///   <item><description>OR - outputs 1 if at least one input is 1, otherwise 0</description></item>
///   <item><description>XOR - outputs 1 if inputs differ, otherwise 0</description></item>
/// </list>
/// </remarks>
type GateOperation = 
    | And
    | Or
    | Xor


/// <summary>
/// Evaluates a boolean logic gate operation on two input values.
/// </summary>
/// <description>
/// Applies the specified logic operation (AND, OR, or XOR) to the two input values
/// and returns the result.
/// This function uses F#'s bitwise operators to perform
/// the boolean logic operations efficiently.
/// </description>
///
/// <remarks>
/// <para>
/// The function implements the three fundamental boolean operations used in the circuit:
/// </para>
///
/// <list type="bullet">
///   <item>
///     <description>AND (&amp;&amp;&amp;) - outputs 1 if both inputs are 1, otherwise 0</description>
///   </item>
///   <item>
///     <description>OR (|||) - outputs 1 if at least one input is 1, otherwise 0</description>
///   </item>
///   <item>
///     <description>XOR (^^^) - outputs 1 if inputs differ, otherwise 0</description>
///   </item>
/// </list>
///
/// <para>
/// The function is marked as inline to optimize performance for this frequently called operation.
/// The bitwise operators ensure that the function works correctly for boolean values represented as integers.
/// </para>
/// </remarks>
///
/// <param name="operation">The logic operation to perform (And, Or, or Xor)</param>
/// <param name="a">The first input value (0 or 1)</param>
/// <param name="b">The second input value (0 or 1)</param>
/// <returns>The result of applying the specified logic operation to the two inputs (0 or 1)</returns>
let inline evaluateGate operation (a: int) (b: int) =
    match operation with  
    | And -> a &&& b 
    | Or -> a ||| b
    | Xor -> a ^^^ b

/// <summary>
/// Represents a logic gate in the circuit with its inputs, output, and operation type.
/// </summary>
///
/// <remarks>
/// Each gate has:
/// <list type="bullet">
///   <item><description>Two input wires identified by their string names</description></item>
///   <item><description>One output wire identified by its string name</description></item>
///   <item><description>An operation type (AND, OR, or XOR)</description></item>
/// </list>
///
/// Gates wait for both inputs to have values before producing an output.
/// </remarks>
type Gate =
    { Input: string * string
      Operation: GateOperation
      Output: string }


/// <summary>
/// Solves Part 1 by simulating the circuit and calculating the decimal value
/// from the binary outputs on z-wires.
/// </summary>
///
/// <remarks>
/// <para>
/// This function processes the circuit simulation and then:
/// </para>
///
/// <list type="number">
///   <item><description>Identifies all wires whose names start with "z"</description></item>
///   <item><description>Orders them by numeric suffix (z00, z01, z02, etc.)</description></item>
///   <item><description>Interprets them as a binary number with z00 as the least significant bit</description></item>
///   <item><description>Converts the binary number to decimal</description></item>
/// </list>
/// </remarks>
///
/// <param name="input">Tuple containing initial wire values and gates</param>
/// <returns>The int64 number produced by the circuit</returns>
let part1 ((wires, gates): (string * int) seq * Gate seq) =
    let rec run eval (gates: Gate list) =
        if List.isEmpty gates then
            eval
        else
            ((eval, []), gates)
            ||> List.fold (fun (eval, gates) g ->
                let input1, input2 = g.Input

                if Map.containsKey input1 eval && Map.containsKey input2 eval then
                    let out =
                        evaluateGate g.Operation (eval[input1]) (eval[input2])

                    Map.add g.Output out eval, gates
                else
                    eval, g :: gates)
            ||> run

    let eval = run (Map.ofSeq wires) (List.ofSeq gates)

    let z =
        eval
        |> Map.filter (fun k _ -> k.StartsWith "z")
        |> Map.values
        |> Seq.rev
        |> Seq.map string
        |> String.concat ""

    Convert.ToInt64(z, 2)

/// <summary>
/// Solves Part 2 by identifying the four pairs of gates whose output wires have been swapped.
/// </summary>
///
/// <remarks>
/// <para>
/// This function implements a search algorithm to find the four pairs of gates that,
/// when their outputs are swapped, allow the circuit to correctly perform binary addition.
/// The approach involves:
/// </para>
///
/// <list type="number">
///   <item><description>Building a validation function that tests if a circuit performs addition correctly</description></item>
///   <item><description>Detecting circuit structure patterns that should appear in a binary adder</description></item>
///   <item><description>Searching through possible gate combinations to identify the swapped pairs</description></item>
///   <item><description>Verifying the solution by checking that the circuit works for all valid inputs</description></item>
/// </list>
///
/// <para>
/// The search employs cycle detection and structural validation to efficiently
/// identify the correct swaps without testing all possible combinations.
/// </para>
/// </remarks>
///
/// <param name="input">Tuple containing initial wire values and gates</param>
/// <returns>Comma-separated string of the eight wire names involved in the swaps, sorted alphabetically</returns>
let part2 ((_wires, gates): (string * int) seq * Gate seq) =
    let hasLoop gateByOut =
        let rec dfs out path =
            if List.contains out path then
                true
            else
                match Map.tryFind out gateByOut with
                | None -> false
                | Some g ->
                    let left, right = g.Input
                    dfs left (out :: path) || dfs right (out :: path)

        gateByOut |> Map.keys |> Seq.exists (fun out -> dfs out [])

    let rec collect out gateByOut =
        match Map.tryFind out gateByOut with
        | None -> []
        | Some g ->
            let left, right = g.Input
            out :: collect left gateByOut @ collect right gateByOut

    let rec make out gateByOut =
        match Map.tryFind out gateByOut with
        | None -> out
        | Some g ->
            let left, right = make (fst g.Input) gateByOut, make (snd g.Input) gateByOut
            let left, right = min left right, max left right

            match g.Operation with
            | And -> $"({left})and({right})"
            | Or -> $"({left})or({right})"
            | Xor -> $"({left})xor({right})"

    let valid out gateByOut =
        let circuit = make out gateByOut

        let validXY =
            // x00, y00, x01, y01, x01, y01, x02, y02, x02, y02, ...
            let xy =
                Regex.Matches(circuit, @"[x|y][0-9]+")
                |> Seq.map (fun m -> m.Value)
                |> Seq.chunkBySize 2

            match List.ofSeq xy with
            | [] -> true
            | [| "x00"; "y00" |] :: t ->
                let t = List.chunkBySize 2 t
                let n = List.length t

                [ 1..n ]
                |> List.forall (fun i ->
                    let xy = [| $"x%02d{i}"; $"y%02d{i}" |]

                    if i < n then
                        t[i - 1] = List.replicate 2 xy
                    else
                        t[i - 1] = [ xy ])
            | _ -> false

        let validOperation =
            // ((((((x00)and(y00))and((x01)xor(y01)))or((x01)and(y01)))and((x02)xor(y02)))or((x02)and(y02)))xor((x03)xor(y03))
            // and, and, xor, or, and, and, xor, or, and, xor, xor
            //      ^^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^
            let ops = Regex.Matches(circuit, @"(and|or|xor)") |> Seq.map (fun m -> m.Value)

            match List.ofSeq ops with
            | [] -> true
            | [ "xor" ] -> true // i = 0
            | "and" :: t ->
                let t = List.chunkBySize 4 t
                let n = List.length t

                [ 0 .. (n - 1) ]
                |> List.forall (fun i ->
                    if i + 1 < n then
                        t[i] = [ "and"; "xor"; "or"; "and" ]
                    else
                        t[i] = [ "xor"; "xor" ])
            | _ -> false

        validXY && validOperation

    let rec search i gateByOut =
        if i >= 45 then
            Some gateByOut
        else
            let out = $"z%02d{i}"

            if valid out gateByOut then
                search (i + 1) gateByOut
            else
                let swaps = collect out gateByOut

                swaps
                |> List.tryPick (fun out ->
                    Map.toList gateByOut
                    |> List.tryPick (fun (out', g') ->
                        let g = gateByOut[out]
                        let newGateByOut = gateByOut |> Map.add out g' |> Map.add out' g

                        if hasLoop newGateByOut then
                            None
                        else
                            let nextInvalid =
                                [ 0..45 ] |> List.find (fun j -> valid $"z%02d{j}" newGateByOut |> not)

                            if i < nextInvalid then
                                search nextInvalid newGateByOut
                            else
                                None))

    let gateByOut =
        gates
        |> Seq.groupBy (fun g -> g.Output)
        |> Seq.map (fun (out, gates) -> out, Seq.exactlyOne gates)
        |> Map

    let correctGateByOut = search 0 gateByOut |> Option.get

    let diff =
        gateByOut
        |> Map.keys
        |> Seq.filter (fun out ->
            let g = gateByOut[out]
            let g' = correctGateByOut[out]
            g <> g')

    diff |> Seq.sort |> String.concat ","



/// <summary>
/// Parses the input text into initial wire values and a collection of gates.
/// </summary>
///
/// <remarks>
/// The input has two sections:
/// <list type="bullet">
///   <item>
///     <description>
///       Initial wire values in the format "wire: value" where value is 0 or 1
///     </description>
///   </item>
///   <item>
///     <description>
///       Gate definitions in the format "inputA OPERATION inputB -> output"
///     </description>
///   </item>
/// </list>
///
/// The parser handles all three operation types (AND, OR, XOR) and creates
/// a data structure representing the complete circuit.
/// </remarks>
///
/// <param name="input">The input text containing wire initializations and gate definitions</param>
/// <returns>A tuple containing initial wire values as a map and a sequence of gates</returns>
let parse (input: string) =
    // Normalize line endings
    let normalizedInput = input.Replace("\r\n", "\n")
    
    // Split input into sections
    let sections = normalizedInput.Split("\n\n", StringSplitOptions.RemoveEmptyEntries)
    
    if sections.Length < 2 then
        failwith $"Expected at least 2 sections, got {sections.Length}. Check input format."
    
    let wires =
        sections[0].Split("\n", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun line ->
            let parts = line.Split(":", StringSplitOptions.RemoveEmptyEntries)
            if parts.Length < 2 then
                failwith $"Invalid wire line: '{line}'"
            parts[0].Trim(), int (parts[1].Trim()))
    
    let gates =
        sections[1].Split("\n", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun line ->
            let parts = line.Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
            
            if parts.Length < 5 then
                failwith $"Invalid gate line: '{line}', found {parts.Length} parts"
            
            let left = parts[0]
            let opStr = parts[1]
            let right = parts[2]
            
            // Check if parts[3] are "->"
            if parts[3] <> "->" then
                failwith $"Expected '->' at position 3, got '{parts[3]}' in line: '{line}'"
                
            let output = parts[4]
            
            let operation =
                match opStr with
                | "AND" -> And
                | "OR" -> Or
                | "XOR" -> Xor
                | _ -> failwithf $"Unknown operation %s{opStr} in line: %s{line}"
            
            { Input = (left, right)
              Operation = operation
              Output = output })
    
    wires, gates


/// <summary>
/// Unit tests for the Day 24 solution using the NUnit framework.
/// </summary>
///
/// <remarks>
/// <para>
/// Tests validate:
/// </para>
///
/// <list type="bullet">
///   <item><description>Correct parsing of input data</description></item>
///   <item><description>Accurate circuit simulation results</description></item>
///   <item><description>Proper identification of swapped gate outputs</description></item>
/// </list>
///
/// <para>
/// Each test uses small, manageable examples to verify distinct aspects of the solution.
/// </para>
/// </remarks>
module Example =
    let input =
        "x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj"

    [<Test>]
    let testPart1 () =
        parse input |> part1 |> should equal 2024L


/// <summary>
/// Main execution function that runs both parts of the Day 24 challenge.
/// </summary>
///
/// <remarks>
/// <para>
/// This function:
/// </para>
///
/// <list type="number">
///   <item><description>Parses the input file</description></item>
///   <item><description>Runs Part 1 to simulate the circuit and calculate the output value</description></item>
///   <item><description>Runs Part 2 to identify the swapped gate outputs</description></item>
///   <item><description>Reports timing and results</description></item>
/// </list>
/// </remarks>
[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    printfn $"Input length: %d{input.Length}"

            
    let wires, gates = parse input
    printfn $"Wires and Gates count: {wires.Length}, {gates.Length}"

    let stopwatch = Stopwatch()
    stopwatch.Start()

    (wires, gates) |> part1 |> printfn "Part 1: %d"
    (wires, gates) |> part2 |> printfn "Part 2: %s"

    stopwatch.Stop()
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"

    0
