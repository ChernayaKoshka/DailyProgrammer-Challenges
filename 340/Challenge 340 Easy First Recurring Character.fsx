open System
let findFirstRepeating (str:string) =
    let input = str.ToCharArray()

    let getDistance (array:char[]) c =
        let firstIndex = Array.IndexOf(array, c)
        if firstIndex+1 > array.Length then None
        else
            let nextIndex = Array.IndexOf(array,c,firstIndex+1)
            if nextIndex = -1 then None
            else Some (c, (firstIndex + nextIndex), firstIndex, nextIndex)

    let l,_,f,s =
        (input
        |> Array.choose (getDistance input)
        |> Array.sortBy (fun (_,x,_,_) -> x)).[0]
    printfn "%c is the first recurring character at %d and %d." l f s

let test() =
    findFirstRepeating "ABCDEBC"
    findFirstRepeating "IKEUNFUVFV"
    findFirstRepeating "PXLJOUDJVZGQHLBHGXIW"
    findFirstRepeating """*l1J?)yn%R[}9~1"=k7]9;0[$"""
    ()
