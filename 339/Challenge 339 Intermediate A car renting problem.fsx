open System

type RentRequest = 
    {start:int; ending:int; length:int}

//assumes sorted
let DoesRequestOverlap req1 req2 =
    req2.start <= req1.ending || req2.ending <= req1.ending

let ShortestLength req1 req2 =
    if req1.length <= req2.length then req1 else req2

let FindMostDates inputs =
    if inputs |> Array.exists (fun x -> x.length <= 0) then failwith "Error: Input start date is greater than or equal to its end date"

    let rec locateOverlaps feasible (inputs:RentRequest[]) index = 
        if index = inputs.Length-1 then
            [|inputs.[index]|] |> Array.append feasible
        else if index > inputs.Length-1 then
            feasible
        else
            let current = inputs.[index]
            let next = inputs.[index+1]
            if DoesRequestOverlap current next then
                let shortest = ShortestLength current next
                locateOverlaps ([|shortest|] |> Array.append feasible) inputs (index+2)
            else
                locateOverlaps ([|current|] |> Array.append feasible ) inputs (index+1)

    let rec sift feasible =
        let sorted =
            feasible
            |> Array.sortBy (fun x -> x.length)
            |> Array.sortBy (fun x -> x.start)

        let next = 
            locateOverlaps [||] sorted 0

        if next.Length = feasible.Length then
            feasible
        else
            sift next

    let most = sift inputs
    printfn "%d" most.Length
    most 
    |> Array.iter (fun x -> printf "(%d,%d) " x.start x.ending)
    printfn ""    

let main() =
    let startDates = (Console.ReadLine().Split [|' '|]) |> Array.map int
    let endDates = (Console.ReadLine().Split [|' '|]) |> Array.map int
    let inputs = Array.map2 (fun x y -> {start=x; ending=y; length=(y-x)} ) startDates endDates
    FindMostDates inputs
    
    let startDates = [|1;12;5;12;13;40;30;22;70;19|]
    let endDates = [|23;99;10;29;25;66;35;33;100;65|]
    let inputs = Array.map2 (fun x y -> {start=x; ending=y; length=(y-x)} ) startDates endDates
    FindMostDates inputs
    ()