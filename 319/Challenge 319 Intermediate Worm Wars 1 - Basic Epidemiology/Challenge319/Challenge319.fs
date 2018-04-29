module Challenge319

#if INTERACTIVE
#r "FSharp.Charting.dll"
#endif

open System
open System.IO
open System.Drawing
open FSharp.Charting

let rec simulate uninfected infected immune (uninfectedToInfectedRate:float) (infectedToImmuneRate:float) (uninfectedToImmuneRate:float) timeLineList =
    match infected with
    | 0 -> (uninfected, infected, immune) :: timeLineList
    | _ ->
           let numUninfectedToImmune = ((float uninfected)*uninfectedToImmuneRate) |> ceil |> int
       
           let numInfectedToImmune = (float infected)*infectedToImmuneRate |> ceil |> int
       
           let numUninfectedToInfected = ((float (uninfected-numUninfectedToImmune))*uninfectedToInfectedRate) |> ceil |> int
       
           let changeInImmune = numInfectedToImmune+numUninfectedToImmune
       
           let changeInInfected = numUninfectedToInfected - numInfectedToImmune
       
           let changeInUninfected = -(numUninfectedToInfected + numUninfectedToImmune)
       
           simulate (uninfected+changeInUninfected) (infected+changeInInfected) (immune+changeInImmune) uninfectedToInfectedRate infectedToImmuneRate uninfectedToImmuneRate ((uninfected, infected, immune) :: timeLineList)
let args = [|"10000"; "10"; "0.01"; "0.01"; "0.015"|]

[<EntryPoint>]
let main argv =
    match argv.Length with
    | 5 -> let populationSize = argv.[0] |> int 
           let initialInfected = argv.[1] |> int
           let uninfectedToInfectedRate = argv.[2] |> float
           let infectedToImmuneRate = argv.[3] |> float
           let uninfectedToImmuneRate = argv.[4] |> float
           let result = simulate (populationSize-initialInfected) initialInfected 0 uninfectedToInfectedRate infectedToImmuneRate uninfectedToImmuneRate [] //[] is an empty list
           let uninfected, infected, immune = result
                                              |> List.rev
                                              |> List.unzip3
           let clean = List.map2 (+) uninfected immune 
           Chart.Combine([Chart.Line(uninfected, Name="Uninfected")
                          Chart.Line(infected,   Name="Infected")
                          Chart.Line(immune,     Name="Immune")
                          Chart.Line(clean,      Name="Clean")])
           |> Chart.WithLegend()
           |> Chart.Save "result.png"
           0
    | _ -> 1
