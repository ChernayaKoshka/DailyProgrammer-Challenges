let alphabet = ['a'..'z']

let alphaIndex (c:char) = alphabet |> List.findIndex ((=) c)

//using a pregenerated table would be faster, but that would take up a lot of space in reddit
let table = [for i in 0..25 -> 
                if i = 0 then alphabet 
                else alphabet.[i..25] @ alphabet.[0..i-1]]
let explode (s:string) = 
    [for c in s -> c]

let encode (table:char list list) (key:string) (text:string) =
    let longKey = [for i in 0..text.Length-1 -> key.[i%key.Length]]
    let text = text |> explode
    List.iter2 (fun keyChar textChar  ->
                    printf "%c" table.[alphaIndex textChar].[alphaIndex keyChar]) longKey text
    printfn ""

let decode (table:char list list) (key:string) (text:string) =
    let longKey = [for i in 0..text.Length-1 -> key.[i%key.Length]]
    let text = text |> explode
    List.iter2 (fun keyChar textChar  ->
                    printf "%c" alphabet.[(table.[alphaIndex keyChar] 
                                           |> List.findIndex ((=) textChar))]) longKey text
    printfn ""

let encodeIt = encode table
encodeIt "snitch" "thepackagehasbeendelivered"
encodeIt "bond" "theredfoxtrotsquietlyatmidnight"
encodeIt "train" "murderontheorientexpress"
encodeIt "garden" "themolessnuckintothegardenlastnight"

let decodeIt = decode table
decodeIt "cloak" "klatrgafedvtssdwywcyty"
decodeIt "python" "pjphmfamhrcaifxifvvfmzwqtmyswst"
decodeIt "moore" "rcfpsgfspiecbcc"