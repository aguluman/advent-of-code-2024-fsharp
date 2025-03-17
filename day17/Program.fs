module day17

open System.Diagnostics
open NUnit.Framework
open FsUnit


// 0 adv: A <- A / 2^■
// 1 bxl: B <- B xor ■
// 2 bst: B <- ■ % 8
// 3 jnz: A ≠ 0 ⇒ ip <- ■
// 4 bxc: B <- B xor C
// 5 out: ■ % 8
// 6 bdv: B <- A / 2^■
// 7 cdv: C <- A / 2^■

type CpuRegister = {
    Accumulator: int64;
    RegisterB: int64;
    RegisterC: int64
}

type CpuState = {
    InstructionPointer: int;
    Register: CpuRegister
    Instructions: int[]
    Outputs: int64 list
}

let rec PowerOfTwo baseValue exponent =
    if exponent = 0L then 1L
    else if exponent % 2L = 0L then PowerOfTwo (baseValue * baseValue) (exponent / 2L)
    else baseValue * PowerOfTwo baseValue (exponent - 1L)


let execute initialregister initialprogram haltcondition =
    let rec step state =
        match haltcondition state with
        | Some out -> out
        | None -> 
            let {
                    InstructionPointer = ip
                    Register = reg
                    Instructions = program
                    Outputs = out } =
                state

            let combo operand =
                match operand with
                | 0
                | 1
                | 2
                | 3 as x -> int64 x
                | 4 -> reg.Accumulator
                | 5 -> reg.RegisterB
                | 6 -> reg.RegisterC
                | x -> failwith $"{x} !?"
            
            let opcode = program[ip]
            let operand = program[ip + 1]

            let newState = 
                match opcode with
                | 0 -> { state with 
                            InstructionPointer = ip + 2
                            Register = 
                                { reg with 
                                    Accumulator = reg.Accumulator / PowerOfTwo 2 (combo operand) } }
                | 1 -> 
                    { state with 
                        InstructionPointer = ip + 2
                        Register = { reg with RegisterB = reg.RegisterB ^^^ operand } }
                | 2 -> 
                    { state with 
                        InstructionPointer = ip + 2
                        Register = { reg with RegisterB = combo operand % 8L } }
                | 3 -> 
                    if reg.Accumulator = 0 then 
                        { state with InstructionPointer = ip + 2}
                    else 
                        { state with InstructionPointer = operand }
                | 4 -> 
                    { state with 
                        InstructionPointer = ip + 2
                        Register = { reg with RegisterB = reg.RegisterB ^^^ reg.RegisterC } }
                | 5 -> 
                    { state with 
                        InstructionPointer = ip + 2
                        Outputs = combo operand % 8L :: out}
                | 6 -> 
                    { state with 
                        InstructionPointer = ip + 2
                        Register = 
                            { reg with 
                                RegisterB = reg.Accumulator / PowerOfTwo 2 (combo operand) } }
                | 7 -> 
                    { state with 
                        InstructionPointer = ip + 2 
                        Register = 
                            { reg with 
                                RegisterC = reg.Accumulator / PowerOfTwo 2 (combo operand) } }
                | x -> failwith $"{x} !?"
            
            step newState
    
    step
        { InstructionPointer = 0
          Register = initialregister
          Instructions = initialprogram
          Outputs = [] }


let outputOnHalt state = 
    if 
        state.InstructionPointer >= state.Instructions.Length 
    then
        state.Outputs 
        |> Array.ofList 
        |> Array.map int 
        |> Array.rev 
        |> Some
    else
        None

let part1 (register: CpuRegister) (program: int[]) =
    let output = execute register program outputOnHalt
    output |> Array.map string |> String.concat ","



let part2 (register: CpuRegister) (program: int[]) =
    // reversing
    //
    // do
    //   B <- A % 8
    //   B <- B xor 5
    //   C <- A / (2 ** B)
    //   B <- B xor 6
    //   A <- A / (2 ** 3)
    //   B <- B xor C
    //   output(B % 8)
    // while (A <> 0)
    //

    let rec find i a =
        if i = 0 then
            Some a
        else
            [ 0..7 ]
            |> List.tryPick (fun j ->
                let accumulator = a * 8L + int64 j
                let output = execute { register with Accumulator = accumulator } program outputOnHalt

                if output = program[(i - 1) ..] then find (i - 1) accumulator else None)

    find program.Length 0 |> Option.get



let parse (input: string) =
    // Normalize line endings and trim
    let input = input.Replace("\r\n", "\n").Trim()
    
    // Find the register section and program section
    let registerLines = 
        input.Split"\n"
        |> Array.takeWhile (fun line -> not (System.String.IsNullOrWhiteSpace line ))
        
    let programLine = 
        input.Split"\n"
        |> Array.skipWhile (fun line -> not (System.String.IsNullOrWhiteSpace line ))
        |> Array.skipWhile System.String.IsNullOrWhiteSpace
        |> Array.tryHead
        
    // Parse registers
    let getRegisterValue (prefix: string) =
        registerLines 
        |> Array.tryFind (fun line -> line.StartsWith prefix)
        |> Option.map (fun line -> line.Substring(prefix.Length).Trim() |> int64)
        |> Option.defaultValue 0L
    
    let register = {
        Accumulator = getRegisterValue "Register A:"
        RegisterB = getRegisterValue "Register B:"
        RegisterC = getRegisterValue "Register C:"
    }
    
    // Parse program
    let program = 
        match programLine with
        | Some line when line.StartsWith "Program:" ->
            line.Substring("Program:".Length).Trim().Split ','
            |> Array.map (fun s -> s.Trim() |> int)
        | _ -> [||]
        
    register, program


module Tests =
    // Example inputs from the challenge
    let exampleInput1 = "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"

    let exampleInput2 = "Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0"

    [<TestFixture>]
    type Day17Tests() =
        
        // Helper function to test register values after execution
        let testRegisterValues (registerA: int64) (registerB: int64) (registerC: int64) (programNumbers: int list) 
                              (expectedA: int64) (expectedB: int64) (expectedC: int64) =
            let register = { Accumulator = registerA; RegisterB = registerB; RegisterC = registerC }
            let program = programNumbers |> Array.ofList
            
            // Custom halt function to capture final register state
            let mutable finalRegister = None
            let haltWithRegisters state = 
                if state.InstructionPointer >= state.Instructions.Length then
                    finalRegister <- Some state.Register
                    Some [||]  // Return empty output to complete execution
                else
                    None
            
            let _ = execute register program haltWithRegisters
            
            match finalRegister with
            | Some regs ->
                regs.Accumulator |> should equal expectedA
                regs.RegisterB |> should equal expectedB
                regs.RegisterC |> should equal expectedC
            | None ->
                failwith "Program didn't halt properly"
        
        // Helper to test output-producing programs
        let testOutput (registerA: int64) (registerB: int64) (registerC: int64) (programNums: int list) (expectedOutput: int list) =
            let register = { Accumulator = registerA; RegisterB = registerB; RegisterC = registerC }
            let program = programNums |> Array.ofList
            let output = execute register program outputOnHalt
            output |> Array.toList |> should equal expectedOutput
        
        [<Test>]
        member _.``C=9, program 2,6 sets B to 1``() =
            testRegisterValues 0L 0L 9L [2; 6] 0L 1L 9L
            
        [<Test>]
        member _.``B=29, program 1,7 sets B to 26``() =
            testRegisterValues 0L 29L 0L [1; 7] 0L 26L 0L
            
        [<Test>]
        member _.``B=2024, C=43690, program 4,0 sets B to 44354``() =
            testRegisterValues 0L 2024L 43690L [4; 0] 0L 44354L 43690L
            
        [<Test>]
        member _.``A=10, program 5,0,5,1,5,4 outputs 0,1,2``() =
            testOutput 10L 0L 0L [5; 0; 5; 1; 5; 4] [0; 1; 2]
            
        [<Test>]
        member _.``A=2024, program 0,1,5,4,3,0 outputs sequence``() =
            testOutput 2024L 0L 0L [0; 1; 5; 4; 3; 0] [4; 2; 5; 6; 7; 7; 7; 7; 3; 1; 0]
            
        [<Test>]
        member _.``Part 1 example - outputs comma-separated sequence``() =
            let register, program = parse exampleInput1
            part1 register program |> should equal "4,6,3,5,6,3,5,2,1,0"
            
        [<Test>]
        member _.``Part 2 example - finds lowest valid register A value``() =
            let register, program = parse exampleInput2
            part2 register program |> should equal 117440L
            
        [<Test>]
        member _.``Register inspection - displays state information``() =
            let register, program = parse exampleInput1
            // This is more for informational purposes
            printfn "\nInitial register state:"
            printfn "  Accumulator: %d" register.Accumulator
            printfn "  Register B: %d" register.RegisterB
            printfn "  Register C: %d" register.RegisterC
            printfn "Program length: %d" program.Length
            printfn "Program content: %s" (program |> Array.map string |> String.concat " ")
            
            // Still need an assertion for the test to be valid
            register.Accumulator |> should equal 729L
            register.RegisterB |> should equal 0L
            register.RegisterC |> should equal 0L
            program |> should haveLength 6
            
        // Test descriptions based on challenge descriptions
        [<Test>]
        member _.``adv instruction divides register A by 2^operand``() =
            testRegisterValues 16L 0L 0L [0; 2] 4L 0L 0L  // A = 16 / 2^2 = 16/4 = 4
            
        [<Test>]
        member _.``bxl instruction performs XOR on register B with literal``() =
            testRegisterValues 0L 10L 0L [1; 7] 0L (10L ^^^ 7L) 0L
            
        [<Test>]
        member _.``bst instruction sets register B to operand mod 8``() =
            testRegisterValues 0L 0L 0L [2; 3] 0L 3L 0L  // 11 % 8 = 3
            
        [<Test>]
        member _.``jnz instruction jumps when A is not zero``() =
            // This program should jump to the "bst" instruction and set B=3
            testRegisterValues 1L 0L 0L [3; 2; 5; 0; 2; 3] 1L 3L 0L
            
        [<Test>]
        member _.``jnz instruction doesn't jump when A is zero``() =
            // This program should execute the "bst" instruction and set B=3
            testRegisterValues 0L 0L 0L [3; 2; 2; 3] 0L 3L 0L
            
        [<Test>]
        member _.``bxc instruction performs XOR on register B with register C``() =
            testRegisterValues 0L 10L 7L [4; 0] 0L (10L ^^^ 7L) 7L

        [<Test>]
        member _.``bdv instruction divides A by 2^operand and stores result in B``() =
            testRegisterValues 16L 0L 0L [6; 2] 16L 4L 0L  // B = 16 / 2^2 = 16/4 = 4, A remains 16
                    
        [<Test>]
        member _.``cdv instruction divides A by 2^operand and stores result in C``() =
            testRegisterValues 16L 0L 0L [7; 2] 16L 0L 4L  // C = 16 / 2^2 = 16/4 = 4, A remains 16


[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let register, program = parse input

    let stopwatch = Stopwatch.StartNew()

    (register, program) ||> part1 |> printfn "Part 1: %s"
    (register, program) ||> part2 |> printfn "Part 2: %d"      

    stopwatch.Stop()
    printfn $"Elapsed time: %.4f{stopwatch.Elapsed.TotalSeconds} seconds"
    0