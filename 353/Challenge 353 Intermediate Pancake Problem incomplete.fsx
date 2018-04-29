let pancakes = [3; 1; 2]

type Stack = int list

let flip (pancakes:Stack) position =
    printfn "flipping -> %A|%d" pancakes position
    let remaining = pancakes.[position+1..]
    let flipped = pancakes.[0..position] |> List.rev
    flipped @ remaining

let flipLargestToTop (pancakes:Stack) =
    let largest = pancakes |> List.max
    let position = pancakes |> List.findIndex ((=) largest)
    printfn "%A : %d -> %d" pancakes largest position
    flip pancakes position

type SortDirection = Forwards|Backwards|NotSorted
let IsStackSorted (pancakes:Stack) =
    let pairwise = pancakes |> List.pairwise
    match pairwise with
    | _ when pairwise |> List.forall (fun (a,b) -> a < b) -> Forwards
    | _ when pairwise |> List.forall (fun (a,b) -> a > b) -> Backwards
    | _ -> NotSorted

type Solution =
    | Solved of (int * Stack)
    | NotSolved of Stack

let rec solve (pancakes:Stack) count =
    match IsStackSorted pancakes with
    | Forwards -> Solved (count, pancakes)
    | Backwards -> Solved (count+1, (pancakes |> List.rev))
    | NotSorted -> solve (pancakes |> flipLargestToTop) (count+1)

let test() =
    solve pancakes 0
            
(*
    [3; 1; 2]
    [2; 1; 3]
    [1; 2; 3]
*)
