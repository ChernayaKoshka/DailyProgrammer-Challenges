let (|Odd|Even|) (u:int) = 
    match u with
    | _ when u%2 = 0 -> Even
    | _ -> Odd
                           
let rec gcd u v = 
    match u with
    | 0 -> v
    | _ when u=v -> u
    | _ -> 
        match v with
        | 0 -> u
        | _ -> 
            match u with
            | Even -> 
              match v with
              | Even -> 2 * gcd (u/2) (v/2) 
              | Odd -> gcd (u/2) v
            | Odd -> 
                match v with
                | Even -> gcd u (v/2)
                | Odd -> 
                    match u > v with
                    | true -> gcd ((u-v)/2) v
                    | false -> gcd ((v-u)/2) u

[<EntryPoint>]
let main argv =
    match argv.Length with
    | 2 -> let u = argv.[0] |> int
           let v = argv.[1] |> int
           let divisor = gcd u v
           printfn "input: %d/%d | gcd: %d | simplified: %d/%d" u v divisor (u/divisor) (v/divisor)
           0
    | _ -> 0