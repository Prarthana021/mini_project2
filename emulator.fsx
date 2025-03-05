open System
open System.Globalization

// Formats binary list as [0;1;0;1] style string
let formatBinary (bin: int list) =
    let s = bin |> List.map string |> String.concat ";"
    "[" + s + "]"

// Removes 0x prefix and converts to uppercase for hex display
let strip0x (hexStr: string) =
    hexStr.Replace("0x", "").ToUpper()

// Recursively prompts user until valid signed byte (-128 to 127) is entered
let rec promptDecimal (message: string) : int =
    printf "%s" message
    let input = Console.ReadLine().Trim()
    match Int32.TryParse(input) with
    | true, value when -128 <= value && value <= 127 -> value
    | _ ->
        printfn "Invalid input. Enter value between -128 and 127."
        promptDecimal message

// Recursively prompts user until valid hex byte (00-FF) is entered
let rec promptHex (message: string) : string =
    printf "%s" message
    let input = Console.ReadLine().Trim().ToUpper()
    match Int32.TryParse(input.Replace("0x", ""), NumberStyles.HexNumber, null) with
    | true, value when 0 <= value && value <= 255 -> "0x" + input.Replace("0x", "")
    | _ ->
        printfn "Invalid hex. Enter value between 0x00 and 0xFF."
        promptHex message

// Converts integer to 8-bit two's complement binary representation
let intToBinary (num: int) : int list =
    let adjusted = if num < 0 then 256 + num else num  // Handle negative numbers
    let rec toBinary n acc bitCount =
        if bitCount = 8 then acc
        else toBinary (n / 2) (n % 2 :: acc) (bitCount + 1)  // Build binary list LSB-first
    toBinary adjusted [] 0

// Converts 8-bit binary list to signed integer (two's complement)
let binaryToInt (bin: int list) : int =
    match bin with
    | [] -> 0
    | signBit::rest ->
        let value = List.fold2 (fun acc bit weight -> acc + bit * weight) 0 rest [64; 32; 16; 8; 4; 2; 1]
        if signBit = 1 then -128 + value else value  // MSB is sign bit

// Converts binary list to unsigned byte value
let binaryToByte (bin: int list) : int =
    List.fold2 (fun acc bit weight -> acc + bit * weight) 0 bin [128; 64; 32; 16; 8; 4; 2; 1]

// Bitwise operations implemented as list transformations
let NOT bin = List.map (fun b -> 1 - b) bin         // Flip all bits
let AND bin1 bin2 = List.map2 (fun a b -> a &&& b) bin1 bin2
let OR bin1 bin2 = List.map2 (fun a b -> a ||| b) bin1 bin2
let XOR bin1 bin2 = List.map2 (fun a b -> a ^^^ b) bin1 bin2

// Binary addition with carry propagation and 8-bit truncation
let ADD bin1 bin2 =
    let rec add bits1 bits2 carry acc =
        match bits1, bits2 with
        | [], [] -> 
            acc  
        | h1::t1, h2::t2 ->
            let sum = h1 + h2 + carry
            let newBit = sum % 2
            let newCarry = sum / 2
            add t1 t2 newCarry (newBit :: acc)
        | _ -> failwith "Mismatched lengths"
    add (List.rev bin1) (List.rev bin2) 0 []

// Subtraction implemented as addition with two's complement negation
let SUB bin1 bin2 = ADD bin1 (ADD (NOT bin2) [0;0;0;0;0;0;0;1])  

// Display results for logical operations (AND/OR/XOR)
let printLogical op aBin aHex bBin bHex resBin =
    let resHex = binaryToByte resBin |> fun x -> x.ToString("X2")  // Force 2-digit hex
    printfn "%s %s %s" op (formatBinary aBin) (strip0x aHex)
    printfn "    %s %s" (formatBinary bBin) (strip0x bHex)
    printfn "________________________________"
    printfn "    %s %s" (formatBinary resBin) resHex

// Display results for arithmetic operations (ADD/SUB)
let printArithmetic op aBin aDec bBin bDec resBin =
    let resDec = binaryToInt resBin  // Use signed conversion
    printfn "%s %s %d" op (formatBinary aBin) aDec
    printfn "    %s %d" (formatBinary bBin) bDec
    printfn "________________________________"
    printfn "    %s %d" (formatBinary resBin) resDec

// Main  interface
let rec main() =
    printfn "\nEnter operation (NOT, AND, OR, XOR, ADD, SUB, QUIT):"
    match Console.ReadLine().Trim().ToUpper() with
    | "QUIT" -> printfn "Goodbye!"
    | "NOT" ->  // Unary operation flow
        let hex = promptHex "Enter hex value (0x00-0xFF): "
        let num = Convert.ToInt32(strip0x hex, 16)
        let res = NOT (intToBinary num)
        printfn "NOT %s %s" (formatBinary (intToBinary num)) (strip0x hex)
        printfn "________________________________"
        printfn "    %s %s" (formatBinary res) ((binaryToByte res).ToString("X2"))
        main()

    | op when ["AND"; "OR"; "XOR"] |> List.contains op ->  // Binary logical operations
        let aHex = promptHex "Enter first hex value (0x00-0xFF): "
        let bHex = promptHex "Enter second hex value (0x00-0xFF): "
        let a = intToBinary (Convert.ToInt32(strip0x aHex, 16))
        let b = intToBinary (Convert.ToInt32(strip0x bHex, 16))
        let res = match op with  // Select operation
                  | "AND" -> AND a b
                  | "OR" -> OR a b
                  | "XOR" -> XOR a b
                  | _ -> failwith "Never happens"
        printLogical op a aHex b bHex res
        main()

    | op when ["ADD"; "SUB"] |> List.contains op ->  // Arithmetic operations
        let aDec = promptDecimal "Enter first decimal number(-128 to 127): "
        let bDec = promptDecimal "Enter second decimal number (-128 to 127): "
        let a = intToBinary aDec
        let b = intToBinary bDec
        let res = match op with  // Select operation
                  | "ADD" -> ADD a b
                  | "SUB" -> SUB a b
                  | _ -> failwith "Never happens"
        printArithmetic op a aDec b bDec res
        main()

    | _ ->
        printfn "Invalid operation"
        main()

// Start the application
main()