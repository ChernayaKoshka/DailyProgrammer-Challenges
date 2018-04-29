let main() =
    let parseInput (input:string) =
        let inputs = input.Split(' ')
        let taps = inputs.[0].[1..inputs.[0].IndexOf(']')-1].Split(',') |> Array.map int
        let operation = inputs.[1]
        let bits = inputs.[2].ToCharArray() 
        let bits = bits |> Array.map (string >> int)
        let cycles = inputs.[3] |> int
        (taps,operation,bits,cycles)

    let rec lfsr (taps:int[]) (operation:string) (cycles:int) (cycle:int) (current:int[]) =
        printf "%d " cycle 
        current |> Array.iter (printf "%d")
        printfn ""
        if cycles = cycle then ()
        else
            let nextBit = 
                let xor = ((taps |> Array.sumBy (fun tap -> current.[tap])) % 2)
                if operation = "XNOR" then 
                    if xor = 1 then 0 else 1
                else xor
            let next = Array.append [|nextBit|] current.[0..current.Length-2]
            lfsr taps operation cycles (cycle+1) next

    [|
    "[0,2] XOR 001 7";
    "[1,2] XOR 001 7";
    "[0,2] XNOR 001 7";
    "[1,2,3,7] XOR 00000001 16";
    "[1,5,6,31] XOR 00000000000000000000000000000001 16"
    |]
    |> Array.iter (fun input ->
        printfn "\r\n%s" input
        let taps,operation,bits,cycles = parseInput input
        lfsr taps operation cycles 0 bits)

