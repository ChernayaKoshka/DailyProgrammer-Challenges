let explode (string:string) = string.ToCharArray() |> List.ofArray
let inputs = 
    [
        [
            "ATCAATATCAA"
            "ATTAAATAACT"
            "AATCCTTAAAC"
            "CTACTTTCTTT"
            "TCCCATCCTTT"
            "ACTTCAATATA"
        ]
        [
            "CTCCATCACAC"
            "AATATCTACAT"
            "ACATTCTCCAT"
            "CCTCCCCACTC"
        ]
        [
            "AACACCCTATA"
            "CTTCATCCACA"
            "TTTCAATTTTC"
            "ACAATCAAACC"
            "ATTCTACAACT"
            "ATTCCTTATTC"
            "ACTTCTCTATT"
            "TAAAACTCACC"
            "CTTTTCCCACC"
            "ACCTTTTCTCA"
            "TACCACTACTT"
        ]
        [
            "ACAAAATCCTATCAAAAACTACCATACCAAT"
            "ACTATACTTCTAATATCATTCATTACACTTT"
            "TTAACTCCCATTATATATTATTAATTTACCC"
            "CCAACATACTAAACTTATTTTTTAACTACCA"
            "TTCTAAACATTACTCCTACACCTACATACCT"
            "ATCATCAATTACCTAATAATTCCCAATTTAT"
            "TCCCTAATCATACCATTTTACACTCAAAAAC"
            "AATTCAAACTTTACACACCCCTCTCATCATC"
            "CTCCATCTTATCATATAATAAACCAAATTTA"
            "AAAAATCCATCATTTTTTAATTCCATTCCTT"
            "CCACTCCAAACACAAAATTATTACAATAACA"
            "ATATTTACTCACACAAACAATTACCATCACA"
            "TTCAAATACAAATCTCAAAATCACCTTATTT"
            "TCCTTTAACAACTTCCCTTATCTATCTATTC"
            "CATCCATCCCAAAACTCTCACACATAACAAC"
            "ATTACTTATACAAAATAACTACTCCCCAATA"
            "TATATTTTAACCACTTACCAAAATCTCTACT"
            "TCTTTTATATCCATAAATCCAACAACTCCTA"
            "CTCTCAAACATATATTTCTATAACTCTTATC"
            "ACAAATAATAAAACATCCATTTCATTCATAA"
            "CACCACCAAACCTTATAATCCCCAACCACAC"
        ]
    ]
    |> List.map (fun sub -> sub |> List.map explode)

let GetCenter inputs =
    let calculateHamming (string1:char list) (string2:char list) =
        List.fold2 (fun acc e1 e2 ->
            if e1 = e2 then acc+1
            else acc) 0 string1 string2
    inputs
    |> List.map (fun s1 ->
        (s1, inputs |> List.sumBy (calculateHamming s1)))
    |> List.maxBy snd

let test() =
    inputs
    |> List.map GetCenter