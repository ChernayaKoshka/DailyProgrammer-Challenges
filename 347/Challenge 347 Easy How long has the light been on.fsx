open System

type TimeOn = 
    {start:int; ending:int;}
//assumes sorted
let DoTimesOverlap req1 req2 =
    req2.start <= req1.ending || req2.ending <= req1.ending

let FindTimeOn inputs =
    let inputs = 
        inputs
        |> Array.sortBy (fun x -> x.start)

    let rec locateOverlaps feasible (inputs:TimeOn[]) index = 
        if index = inputs.Length-1 then
            [|inputs.[index]|] |> Array.append feasible
        else if index > inputs.Length-1 then
            feasible
        else
            let current = inputs.[index]
            let next = inputs.[index+1]
            if DoTimesOverlap current next then
                locateOverlaps ([|{start=current.start;ending=next.ending;}|] |> Array.append feasible) inputs (index+2)
            else
                locateOverlaps ([|current|] |> Array.append feasible ) inputs (index+1)

    let rec sift feasible =
        let next =  locateOverlaps [||] feasible 0

        if next.Length = feasible.Length then
            feasible
        else
            sift next

    sift inputs
    |> Array.sumBy(fun x -> (x.ending-x.start))
    |> printfn "On for: %d"
    printfn ""    

let main() =
    let startTimes = [|2;3;1;6|]
    let endTimes = [|4;6;3;8|]
    Array.map2 (fun x y -> {start=x; ending=y;} ) startTimes endTimes
    |> Array.sortBy (fun x -> x.start)
    |> FindTimeOn

    let startDates = [|6;5;8;5;4|]
    let endDates = [|8;8;9;7;7|]
    Array.map2 (fun x y -> {start=x; ending=y;} ) startDates endDates
    |> Array.sortBy (fun x -> x.start)
    |> FindTimeOn

    let startDates = [|15;13;9;3;17;9;17;4;5;4;5;13;2;15;13|]
    let endDates = [|18;16;12;4;20;11;18;5;6;5;6;16;3;17;14|]
    Array.map2 (fun x y -> {start=x; ending=y;} ) startDates endDates
    |> Array.sortBy (fun x -> x.start)
    |> FindTimeOn
    ()