open System
open System.IO

type Direction = UP | DOWN | LEFT | RIGHT

let dir2Str dir =
    match dir with
    | UP -> "U"
    | DOWN -> "D"
    | LEFT -> "L"
    | RIGHT -> "R"
let rec fillSpiral (spiral:int[,]) num square goal curX curY direction =
    match num with
    | x when num=goal+1 -> spiral
    | _ -> 
           spiral.[curX,curY] <- num
           let newDirection = match direction with
                              | UP -> match curY with
                                      | a when curY=0 -> RIGHT
                                      | b when spiral.[curX,(curY-1)] <> -1 -> RIGHT
                                      | _ -> UP
                              | DOWN -> match curY with
                                        | a when curY=square-1 -> LEFT
                                        | b when spiral.[curX,(curY+1)] <> -1 -> LEFT
                                        | _ -> DOWN
                              | LEFT -> match curX with
                                        | a when curX=0 -> UP
                                        | b when spiral.[curX-1,curY] <> -1 -> UP
                                        | _ -> LEFT
                              | RIGHT -> match curX with
                                         | a when curX=square-1 -> DOWN
                                         | b when spiral.[curX+1,curY] <> -1 -> DOWN
                                         | _ -> RIGHT
           let nextX = match newDirection with
                       | LEFT -> curX-1
                       | RIGHT -> curX+1
                       | _ -> curX
           let nextY = match newDirection with
                       | UP -> curY-1
                       | DOWN -> curY+1
                       | _ -> curY
           fillSpiral spiral (num+1) square goal nextX nextY newDirection

let makeSpiral num =
    let square = num*num
    let matrix = Array2D.init num num (fun x y -> -1)
    let spiral = fillSpiral matrix 1 num square 0 0 RIGHT
    let alignment = ((log (square |> double))) |> int

    for y in [0..num-1] do
        for x in [0..num-1] do
            printf "%*d" alignment spiral.[x,y]
        printfn ""
    ()

[<EntryPoint>]
let main argv = 
    match argv.Length with
    | 1 -> let num = argv.[0] |> int
           makeSpiral num
           0
    | 0 | _ -> 1