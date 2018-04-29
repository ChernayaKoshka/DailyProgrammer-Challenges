open System.IO
let numbers =
    [
    [[' ';'_';' ';]
     ['|';' ';'|';]
     ['|';'_';'|';]]

    [[' ';' ';' ';]
     [' ';' ';'|';]
     [' ';' ';'|';]]

    [[' ';'_';' ';]
     [' ';'_';'|';]
     ['|';'_';' ';]]

    [[' ';'_';' ';]
     [' ';'_';'|';]
     [' ';'_';'|';]]

    [[' ';' ';' ';]
     ['|';'_';'|';]
     [' ';' ';'|';]]

    [[' ';'_';' ';]
     ['|';'_';' ';]
     [' ';'_';'|';]]

    [[' ';'_';' ';]
     ['|';'_';' ';]
     ['|';'_';'|';]]

    [[' ';'_';' ';]
     [' ';' ';'|';]
     [' ';' ';'|';]]

    [[' ';'_';' ';]
     ['|';'_';'|';]
     ['|';'_';'|';]]

    [[' ';'_';' ';]
     ['|';'_';'|';]
     [' ';'_';'|';]]
    ]
    |> List.map array2D
    
let slice (arr:char[][]) =
    let arr2d = Array2D.initBased 0 0 3 arr.[0].Length (fun y x -> arr.[y].[x])
    [for i in 0..((arr.[0].Length-1)/3) ->
        arr2d.[0..2,i*3..i*3+2]]

let digitize path = 
    File.ReadAllLines(path) 
    |> Array.map (fun s -> s.ToCharArray())
    |> slice
    |> List.map (fun digit ->
        numbers
        |> List.findIndex ((=)digit)
        |> string)
    |> List.reduce (+)

["1.txt";"2.txt";"3.txt";"4.txt"]
|> List.map digitize
|> List.iter (printfn "%s")