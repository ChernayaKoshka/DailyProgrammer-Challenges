#if INTERACTIVE
#else
module Challenge321
#endif

open System
open System.IO

let coprimes = [|3;5;7;11;15;17;19;21;23;25|]

let alpha = [|'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z'|]

let AlphaIndex (x:char) = Array.IndexOf(alpha,x)

let IntToAlpha (x:int) = alpha.[x]

let ToUpper (x:string) = x.ToUpper()

let ToLower (x:string) = x.ToLower()

let explode word =  [|for c in word -> c|]

let convertWord aCoprimeIndex b (letters:char[])  =
    letters
    |> Array.map (fun x -> ((coprimes.[aCoprimeIndex]*(AlphaIndex x)+b)%26) |> IntToAlpha)
    |> String.Concat
    |> ToLower

let checkWord (dictionary:string[]) (word:string) =
    Array.exists ((=) word) dictionary

let convertSentence aCoprimeIndex b (sentence:string[]) =
    Array.map (fun x -> explode x  |> convertWord aCoprimeIndex b) sentence
let convertCheckSentence aCoprimeIndex b dictionary (sentence:string[])  =
    let converted = convertSentence aCoprimeIndex b sentence
    match Array.TrueForAll(converted, (fun x -> checkWord dictionary x)) with
    | false -> None
    | true -> Some converted

let scoreCandidate (dictionary:string[]) (candidate:string[]) =
    candidate 
    |> Array.filter (checkWord dictionary)
    |> Array.length

let crackSentence (dictionary:string[]) (sentence:string) =
    let words = sentence.Split(' ')
    [|for aCoprimeIndex in [0..9] do
         for b in [0..25] do
             let converted = convertCheckSentence aCoprimeIndex b dictionary words
             match converted with
             | None -> ()
             | Some x -> yield (aCoprimeIndex, b, x)|]

let printCandidates (candidates:(int*int*string[])[]) =
    for x in [0..candidates.Length-1] do
        let a, b, c = candidates.[x]
        printfn "[%d,%d]Candidate # %d:%s" a b x (c |> Array.map ((+) " ") |> String.Concat) 

let crackPrint dictionary (sentence:string) = 
    printfn "Input: %s" sentence
    sentence
    |> ToLower
    |> crackSentence dictionary
    |> printCandidates

let testRun dictionary = 
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    "DZ"
    |> crackPrint dictionary
    printfn "Timer: %dms elapsed\r\n" stopWatch.ElapsedMilliseconds

    "NLWC WC M NECN"
    |> crackPrint dictionary
    printfn "Timer: %dms elapsed\r\n" stopWatch.ElapsedMilliseconds

    "YEQ LKCV BDK XCGK EZ BDK UEXLVM QPLQGWSKMB"
    |> crackPrint dictionary
    printfn "Timer: %dms elapsed\r\n" stopWatch.ElapsedMilliseconds

    "NH WRTEQ TFWRX TGY T YEZVXH GJNMGRXX STPGX NH XRGXR TX QWZJDW ZK WRNUZFB P WTY YEJGB ZE RNSQPRY XZNR YJUU ZSPTQR QZ QWR YETPGX ZGR NPGJQR STXQ TGY URQWR VTEYX WTY XJGB"
    |> crackPrint dictionary
    printfn "Timer: %dms elapsed\r\n" stopWatch.ElapsedMilliseconds

    stopWatch.Stop()

[<EntryPoint>]
let main argv =
    let dictionary = File.ReadAllLines("dictionary.txt")
    match argv.Length with
    | 0 -> testRun dictionary
           0
    | 1 -> crackPrint dictionary argv.[0]
           0
    | 3 -> let aCoprimeIndex = argv.[1] |> int
           let b = argv.[2] |> int
           let words = (argv.[0] |> ToLower).Split(' ')
           let converted = convertSentence aCoprimeIndex b words
                           |> Array.map ((+) " ")
                           |> String.Concat
           printfn "[%d,%d]Output:%s" aCoprimeIndex b (converted |> ToUpper)
           0
    | _ -> 1