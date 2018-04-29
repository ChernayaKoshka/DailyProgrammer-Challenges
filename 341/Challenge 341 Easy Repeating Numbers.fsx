let num = "9870209870409898"

let CountPermutationsOfLength (num:string) minLength =
    [for i in [0..num.Length-1] ->
        [for z in [0..num.Length-i] ->
             num.Substring(i,z)]
        |> List.filter (fun x -> x.Length >= minLength || x.Length < 0)]
    |> List.filter ((<>) [])
    |> List.collect id
    |> List.countBy id
    |> List.filter (snd >> (<) 1)
    |> List.sortByDescending (fun (str,_) -> str.Length)

let PrintPermutations (perms:(string*int) list) =
    perms |> List.iter (fun (str,count) -> printf "%s:%d " str count)
    printfn ""