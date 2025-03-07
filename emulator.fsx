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
let intToBinary num =
    let adjusted = if num < 0 then 256 + num else num
    List.unfold (fun (n, count) ->
        if count = 8 then None
        else Some(n % 2, (n / 2, count + 1))
    ) (adjusted, 0)
    |> List.rev

// Converts 8-bit binary list to signed integer (two's complement)
let binaryToInt (bin: int list) : int =
    match bin with
    | [] -> 0
    | signBit :: rest ->
        let value = List.fold2 (fun acc bit weight -> acc + bit * weight) 0 rest [64; 32; 16; 8; 4; 2; 1]
        if signBit = 1 then -128 + value else value

// Converts binary list to unsigned byte value
let binaryToByte (bin: int list) : int =
    List.fold2 (fun acc bit weight -> acc + bit * weight) 0 bin [128; 64; 32; 16; 8; 4; 2; 1]

// Logical operations 
let NOT bin = List.map (fun b -> 1 - b) bin
let AND bin1 bin2 = List.map2 (fun a b -> a &&& b) bin1 bin2
let OR bin1 bin2 = List.map2 (fun a b -> a ||| b) bin1 bin2
let XOR bin1 bin2 = List.map2 (fun a b -> a ^^^ b) bin1 bin2

// Binary addition with carry propagation 
let ADD bin1 bin2 =
    let rec add bits1 bits2 carry acc =
        match bits1, bits2 with
        | [], [] -> acc  
        | h1 :: t1, h2 :: t2 ->
            let sum = h1 + h2 + carry
            add t1 t2 (sum / 2) (sum % 2 :: acc)
        | _ -> failwith "Mismatched lengths"
    add (List.rev bin1) (List.rev bin2) 0 []

// Subtraction implemented as addition with two's complement negation
let SUB bin1 bin2 = ADD bin1 (ADD (NOT bin2) [0;0;0;0;0;0;0;1])

// print function for both operation types
let printResult op aBin aVal bBin bVal resBin (formatVal: int -> string) =
    let resVal = binaryToInt resBin |> formatVal
    printfn "%s %s %s" op (formatBinary aBin) aVal
    printfn "    %s %s" (formatBinary bBin) bVal
    printfn "________________________________"
    printfn "    %s %s" (formatBinary resBin) resVal

// Main interface
let rec main() =
    printfn "\nEnter operation (NOT, AND, OR, XOR, ADD, SUB, QUIT):"
    match Console.ReadLine().Trim().ToUpper() with
    | "QUIT" -> printfn "Goodbye!"
    | "NOT" ->
        let hex = promptHex "Enter hex value (0x00-0xFF): "
        let num = Convert.ToInt32(strip0x hex, 16)
        let res = NOT (intToBinary num)
        printfn "NOT %s %s" (formatBinary (intToBinary num)) (strip0x hex)
        printfn "________________________________"
        printfn "    %s %s" (formatBinary res) ((binaryToByte res).ToString("X2"))
        main()
    | op when ["AND"; "OR"; "XOR"] |> List.contains op ->
        let aHex = promptHex "Enter first hex value (0x00-0xFF): "
        let bHex = promptHex "Enter second hex value (0x00-0xFF): "
        let a = intToBinary (Convert.ToInt32(strip0x aHex, 16))
        let b = intToBinary (Convert.ToInt32(strip0x bHex, 16))
        let res = match op with
                  | "AND" -> AND a b
                  | "OR"  -> OR a b
                  | "XOR" -> XOR a b
                  | _ -> failwith "Never happens"
        printResult op a (strip0x aHex) b (strip0x bHex) res (fun x -> (x &&& 0xFF).ToString("X2"))
        main()
    | op when ["ADD"; "SUB"] |> List.contains op ->
        let aDec = promptDecimal "Enter first decimal number(-128 to 127): "
        let bDec = promptDecimal "Enter second decimal number (-128 to 127): "
        let a = intToBinary aDec
        let b = intToBinary bDec
        let res = match op with
                  | "ADD" -> ADD a b
                  | "SUB" -> SUB a b
                  | _ -> failwith "Never happens"
        printResult op a (string aDec) b (string bDec) res (string)
        main()
    | _ ->
        printfn "Invalid operation"
        main()

// Start the application
main()

