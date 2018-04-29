open System
open System.IO

[<EntryPoint>]
let main argv =
    let explode (s:string) = 
        [for c in s -> c]
    
    let rec collatz (current : char List) = //recursive function denoted by the 'rec' keyword
        printfn "%s" (current |> Array.ofList |> String.Concat) //convert list to string
        
        match current with
        | [_] -> 
            current
        | _ -> 
            match current.Item(0) with //rules
            | 'a' -> ['b';'c']
            | 'b' -> ['a']
            | 'c' -> ['a';'a';'a']
            |> List.append current.[2..] //append new list from matching pattern to the third item in the old list
            |> collatz //recurse

    "aaa"
    |> explode
    |> collatz
    |> ignore

    printfn ""

    "aaaaaaa"
    |> explode
    |> collatz
    |> ignore

    printfn ""

    "aaaaaaaaaaaaaa"
    |> explode
    |> collatz
    |> ignore

    0 // return an integer exit code
