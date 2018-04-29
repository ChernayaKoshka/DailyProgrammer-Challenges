let kolakoskiNaive num =
    let rec genSequence (num:uint64) (current:int list) iteration =
        if uint64(current.Length) >= num then current
        else genSequence num (current @ (List.init current.[iteration-1] (fun _ -> (iteration % 2 + 1)))) (iteration+1)

    let sequence = genSequence num [1;2;2] 3
    let countOfOnes = sequence |> List.sumBy (fun i -> i%2)
    (countOfOnes,sequence.Length-countOfOnes)
let test = 
    [10UL;100UL;1000UL;1000000UL;100000000UL]//1000000000000UL;100000000000000UL]
    |> List.map kolakoskiNaive
    |> List.iter (printfn "%A")