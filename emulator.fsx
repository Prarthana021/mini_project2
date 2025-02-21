open System
open System.Globalization

let formatBinary (bin: int list) =
    let s = bin |> List.map string |> String.concat ";"
    "[" + s + "]"

let strip0x (hexStr: string) =
    if hexStr.StartsWith("0x", StringComparison.OrdinalIgnoreCase) then
        hexStr.Substring(2)
    else
        hexStr

let printArithmetic (op: string) (aBin: int list) (aDec: int)
                    (bBin: int list) (bDec: int)
                    (resBin: int list) (resDec: int) =
    printfn "%s %s %d" op (formatBinary aBin) aDec
    printfn "    %s %d" (formatBinary bBin) bDec
    printfn "________________________________"
    printfn "    %s %d" (formatBinary resBin) resDec


let printLogical (op: string)
                 (aBin: int list) (aHex: string)
                 (bBin: int list) (bHex: string)
                 (resBin: int list) (resHex: string) =
    printfn "%s %s %s" op (formatBinary aBin) (strip0x aHex)
    printfn "    %s %s" (formatBinary bBin) (strip0x bHex)
    printfn "________________________________"
    printfn "    %s %s" (formatBinary resBin) (strip0x resHex)


let rec promptDecimal (message: string) : int =
    printf "%s" message
    let input = Console.ReadLine().Trim()
    match System.Int32.TryParse(input) with
    | true, value when value >= -128 && value <= 127 ->
        value
    | _ ->
        printfn "Invalid decimal input. Please enter a number between -128 and 127."
        promptDecimal message

let rec promptHex (message: string) : string =
    printf "%s" message
    let raw = Console.ReadLine().Trim()
    let input = raw.Replace("0x", "")
    match System.Int32.TryParse(input, NumberStyles.HexNumber, CultureInfo.InvariantCulture) with
    | true, value when value >= 0 && value <= 255 ->
        "0x" + input
    | _ ->
        printfn "Invalid hex input. Please enter a valid hex number between 0x00 and 0xFF."
        promptHex message

//   Emulator

// Convert an integer to 8-bit binary
let intToBinary (num: int) : int list =
    let adjusted = if num < 0 then 256 + num else num
    let rec toBinary num acc bitCount =
        if bitCount = 8 then acc
        else toBinary (num / 2) ((num % 2) :: acc) (bitCount + 1)
    toBinary adjusted [] 0

// Convert 8-bit binary to an integer (unsigned)
let binaryToInt (bin: int list) : int =
    let values = [128; 64; 32; 16; 8; 4; 2; 1]
    List.fold2 (fun acc b v -> acc + (b * v)) 0 bin values

// Bitwise NOT on 8-bit
let bitwiseNot (bin: int list) : int list =
    bin |> List.map (fun b -> if b = 1 then 0 else 1)

// 8-bit addition (mod 256)
let addBinary (a: int list) (b: int list) : int list =
    let rec addHelper a b carry acc =
        match a, b with
        | [], [] -> acc
        | ah::at, bh::bt ->
            let sum = ah + bh + carry
            let newBit = sum % 2
            let newCarry = sum / 2
            addHelper at bt newCarry (newBit :: acc)
        | _ -> failwith "Binary lists must be of equal length"
    addHelper (List.rev a) (List.rev b) 0 []

// Subtraction: a - b = a + (two's complement of b)
let subBinary (a: int list) (b: int list) : int list =
    let bNot = bitwiseNot b
    let bTwoComp = addBinary bNot [0;0;0;0;0;0;0;1]
    addBinary a bTwoComp

// Logical operation on 8-bit unsigned
let bitwiseOp (op: int -> int -> int) (a: int list) (b: int list) : int list =
    List.map2 op a b

// Convert hex string  to 8-bit binary
let hexToBinary (hex: string) : int list =
    let cleaned = hex.Trim().Replace("0x", "")
    let num = Convert.ToInt32(cleaned, 16) % 256
    intToBinary num

// Convert binary to hex 
let binaryToHex (bin: int list) : string =
    let num = binaryToInt bin
    sprintf "%X" num  

// Main Loop using Tail recursion
//emulator.fsx/Users/roublenepalgmail.com/Desktop/Spring_Sem/programming /mini_project2/emulator.fsx
let rec main () =
    printfn ""
    printfn "Enter the operation you want to perform (NOT, OR, AND, XOR, ADD, SUB or QUIT)?"
    let operation = Console.ReadLine().Trim().ToUpper()

    match operation with
    | "ADD" | "SUB" ->
        // Arithmetic: decimal input
        let aDec = promptDecimal "Enter first decimal value (-128 to 127): "
        let bDec = promptDecimal "Enter second decimal value (-128 to 127): "
        let aBin = intToBinary aDec
        let bBin = intToBinary bDec
        let resBin =
            match operation with
            | "ADD" -> addBinary aBin bBin
            | "SUB" -> subBinary aBin bBin
            | _     -> failwith "Invalid operation"

        // Convert to signed result
        let rawResult = binaryToInt resBin
        let resDec = if rawResult > 127 then rawResult - 256 else rawResult

        printArithmetic operation aBin aDec bBin bDec resBin resDec
        main ()

    | "AND" | "OR" | "XOR" ->
        // Logical: hex input
        let aHexInput = promptHex "Enter first hex value (0x0 to 0xFF): "
        let bHexInput = promptHex "Enter second hex value (0x0 to 0xFF): "
        let aBin = hexToBinary aHexInput
        let bBin = hexToBinary bHexInput
        let resBin =
            match operation with
            | "AND" -> bitwiseOp (fun x y -> if x = 1 && y = 1 then 1 else 0) aBin bBin
            | "OR"  -> bitwiseOp (fun x y -> if x = 1 || y = 1 then 1 else 0) aBin bBin
            | "XOR" -> bitwiseOp (fun x y -> if x <> y then 1 else 0) aBin bBin
            | _     -> failwith "Invalid logical operation"

        let resHex = binaryToHex resBin

        printLogical operation aBin aHexInput bBin bHexInput resBin resHex
        main ()

    | "NOT" ->
        // Single hex input
        let aHexInput = promptHex "Enter hex value (0x0 to 0xFF): "
        let aBin = hexToBinary aHexInput
        let resBin = bitwiseNot aBin
        let resHex = binaryToHex resBin

        // Formatting
        printfn "%s %s %s" operation (formatBinary aBin) (strip0x aHexInput)
        printfn "________________________________"
        printfn "    %s %s" (formatBinary resBin) (strip0x resHex)

        main ()

    | "QUIT" ->
        printfn "Exiting..."

    | _ ->
        printfn "Invalid operation. Please try again."
        main ()

// Start
main ()
