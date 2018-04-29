open System.IO

//read csv file and convert to tuples of (winner,loser)
let parse file =
    File.ReadAllLines(file).[1..] //don't need header 0=team1,1=team1score,2=team2,3=team2score
    |> Array.map (fun line -> 
        let split = line.Split(',')
        if ((split.[1]|>int) > (split.[3]|>int)) then 
            (split.[0],split.[2])
        else
            (split.[2],split.[0]))
    |> List.ofArray

let rec locate (pool:(string*string) list) (winners:string list) =
    let (transitiveWinners,remaining) =
        pool
        |> List.partition (fun (_,t2) ->
            match winners |> List.tryFind ((=)t2) with
            | Some _ -> true
            | _ -> false)

    let transitiveWinners = 
        transitiveWinners 
        |> List.map fst

    match transitiveWinners.Length with
    | 0 -> (winners |> List.distinct).Length
    | _ -> locate remaining (transitiveWinners @ winners)
    
let games = parse (__SOURCE_DIRECTORY__ + "\\scores.csv")
let numTeams = 
    let a,b = games |> List.unzip
    ((a@b)
    |> List.distinct).Length

let numTransitiveWinners = locate games ["Villanova"]

printfn "%d/%d (%d%%) are transitive winners." numTransitiveWinners numTeams (int ((float numTransitiveWinners)/(float numTeams)*100.00))
