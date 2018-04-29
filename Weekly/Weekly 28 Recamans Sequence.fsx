let inputs = [5; 15; 25; 100; 1005; 10005; 100000]

let recamans digit =
    [1..digit]
    |> List.fold (fun (current:int list) position ->
         let sub = current.Head - position
         let add = current.Head + position
         if (sub > 0) && (List.tryFind ((=) sub) current = None) then
            sub :: current
         else
            add :: current) [0]
    |> List.rev

let test() =
    let r = recamans (inputs |> List.max)
    inputs |> List.map (fun i -> r.[i])