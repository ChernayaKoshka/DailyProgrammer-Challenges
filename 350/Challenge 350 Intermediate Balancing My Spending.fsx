let sequences =
    [
        [0; -3; 5; -4; -2; 3; 1; 0;]
        [3; -2; 2; 0; 3; 4; -6; 3; 5; -4; 8;]
        [9; 0; -5; -4; 1; 4; -4; -9; 0; -7; -1;]
        [9; -7; 6; -8; 3; -9; -5; 3; -6; -8; 5;]
    ]

//O(n^2)
let main() =
    for sequence in sequences do
        let _,_,feasible = 
            sequence
            |> List.fold (fun (index,leftSum,feasible) elem ->
                let rightSum = sequence.[index+1..] |> List.sum
                if leftSum = rightSum then (index+1, leftSum+elem, (index :: feasible))
                else (index+1, leftSum+elem, feasible)) (0,0,[])
        feasible |> List.rev |> List.iter (printf "%d ")
        printfn ""
    ()

//O(n)
let main2() =
    for sequence in sequences do
        let _,_,_,feasible = 
            sequence
            |> List.fold (fun (index,leftSum,rightSum,feasible) elem ->
                let leftSum = leftSum+elem
                let rightSum = rightSum-elem
                if leftSum = rightSum then (index+1, leftSum+elem, rightSum-elem, (index :: feasible))
                else (index+1, leftSum+elem,rightSum-elem, feasible)) (0,0,(sequence|>List.sum),[])
        feasible |> List.rev |> List.iter (printf "%d ")
        printfn ""
    ()