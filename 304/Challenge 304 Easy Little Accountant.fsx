open System
open System.IO

type Journal = 
    { account : int
      period : string
      debit : int
      credit : int }

type Account = 
    { account : int
      label : string }

let Accounts = 
    [ { Account.account = 1000
        Account.label = "Cash" }
      { Account.account = 1020
        Account.label = "Account Receivables" }
      { Account.account = 1100
        Account.label = "Lab Equipement" }
      { Account.account = 1110
        Account.label = "Office Supplies" }
      { Account.account = 2000
        Account.label = "Notes Payables" }
      { Account.account = 2010
        Account.label = "Account Payables" }
      { Account.account = 2110
        Account.label = "Utilities Payables" }
      { Account.account = 3000
        Account.label = "Common Stock" }
      { Account.account = 4000
        Account.label = "Commercial Revenue" }
      { Account.account = 4090
        Account.label = "Unearned Revenue" }
      { Account.account = 5000
        Account.label = "Direct Labor" }
      { Account.account = 5100
        Account.label = "Consultants" }
      { Account.account = 5500
        Account.label = "Misc Costs" }
      { Account.account = 7140
        Account.label = "Rent" }
      { Account.account = 7160
        Account.label = "Telephone" }
      { Account.account = 9090
        Account.label = "Dividends" } ]

let Journals = 
    [ { Journal.account = 1000
        Journal.period = "JAN-16"
        Journal.debit = 100000
        Journal.credit = 0 }
      { Journal.account = 3000
        Journal.period = "JAN-16"
        Journal.debit = 0
        Journal.credit = 100000 }
      { Journal.account = 7140
        Journal.period = "JAN-16"
        Journal.debit = 36000
        Journal.credit = 0 }
      { Journal.account = 1000
        Journal.period = "JAN-16"
        Journal.debit = 0
        Journal.credit = 36000 }
      { Journal.account = 1100
        Journal.period = "FEB-16"
        Journal.debit = 80000
        Journal.credit = 0 }
      { Journal.account = 1000
        Journal.period = "FEB-16"
        Journal.debit = 0
        Journal.credit = 60000 }
      { Journal.account = 2000
        Journal.period = "FEB-16"
        Journal.debit = 0
        Journal.credit = 20000 }
      { Journal.account = 1110
        Journal.period = "FEB-16"
        Journal.debit = 17600
        Journal.credit = 0 }
      { Journal.account = 2010
        Journal.period = "FEB-16"
        Journal.debit = 0
        Journal.credit = 17600 }
      { Journal.account = 1000
        Journal.period = "MAR-16"
        Journal.debit = 28500
        Journal.credit = 0 }
      { Journal.account = 4000
        Journal.period = "MAR-16"
        Journal.debit = 0
        Journal.credit = 28500 }
      { Journal.account = 2010
        Journal.period = "MAR-16"
        Journal.debit = 17600
        Journal.credit = 0 }
      { Journal.account = 1000
        Journal.period = "MAR-16"
        Journal.debit = 0
        Journal.credit = 17600 }
      { Journal.account = 5000
        Journal.period = "APR-16"
        Journal.debit = 19100
        Journal.credit = 0 }
      { Journal.account = 1000
        Journal.period = "APR-16"
        Journal.debit = 0
        Journal.credit = 19100 }
      { Journal.account = 1000
        Journal.period = "APR-16"
        Journal.debit = 32900
        Journal.credit = 0 }
      { Journal.account = 1020
        Journal.period = "APR-16"
        Journal.debit = 21200
        Journal.credit = 0 }
      { Journal.account = 4000
        Journal.period = "APR-16"
        Journal.debit = 0
        Journal.credit = 54100 }
      { Journal.account = 1000
        Journal.period = "MAY-16"
        Journal.debit = 15300
        Journal.credit = 0 }
      { Journal.account = 1020
        Journal.period = "MAY-16"
        Journal.debit = 0
        Journal.credit = 15300 }
      { Journal.account = 1000
        Journal.period = "MAY-16"
        Journal.debit = 4000
        Journal.credit = 0 }
      { Journal.account = 4090
        Journal.period = "MAY-16"
        Journal.debit = 0
        Journal.credit = 4000 }
      { Journal.account = 1110
        Journal.period = "JUN-16"
        Journal.debit = 5200
        Journal.credit = 0 }
      { Journal.account = 2010
        Journal.period = "JUN-16"
        Journal.debit = 0
        Journal.credit = 5200 }
      { Journal.account = 5100
        Journal.period = "JUN-16"
        Journal.debit = 19100
        Journal.credit = 0 }
      { Journal.account = 1000
        Journal.period = "JUN-16"
        Journal.debit = 0
        Journal.credit = 19100 }
      { Journal.account = 4120
        Journal.period = "JUN-16"
        Journal.debit = 5000
        Journal.credit = 0 }
      { Journal.account = 1000
        Journal.period = "JUN-16"
        Journal.debit = 0
        Journal.credit = 5000 }
      { Journal.account = 7160
        Journal.period = "JUL-16"
        Journal.debit = 2470
        Journal.credit = 0 }
      { Journal.account = 2010
        Journal.period = "JUL-16"
        Journal.debit = 0
        Journal.credit = 2470 }
      { Journal.account = 5500
        Journal.period = "JUL-16"
        Journal.debit = 3470
        Journal.credit = 0 }
      { Journal.account = 1000
        Journal.period = "JUL-16"
        Journal.debit = 0
        Journal.credit = 3470 } ]


type Period = 
    { month : int
      day : int }

let StringToPeriod(str : string) = 
    let uppercase (x : string) = x.ToUpper()
    let monthToInt (month : string) = 
        match month |> uppercase with
        | "JAN" -> 1
        | "FEB" -> 2
        | "MAR" -> 3
        | "APR" -> 4
        | "MAY" -> 5
        | "JUN" -> 6
        | "JUL" -> 7
        | "AUG" -> 8
        | "SEP" -> 9
        | "OCT" -> 10
        | "NOV" -> 11
        | "DEC" -> 12
        | _ -> 0
    
    let split = str.Split '-'
    let month = split.[0] |> monthToInt
    let day = split.[1] |> int
    { Period.month = month
      Period.day = day }

let FilterPeriod (x : Period) (y : Period) = 
    (x.month >= y.month && x.day >= y.day)

let consolidate journals account = 
    (account, (List.filter (fun (x : Journal) -> x.account = account.account) journals))

let consolidateAll journals (accounts : Account List) = 
    List.map (fun x -> consolidate journals x) accounts |> List.filter (fun x -> (snd x) <> [])

let sumJournals (journals : Journal List) = 
    let debit = List.sumBy (fun x -> x.debit) journals
    let credit = List.sumBy (fun x -> x.credit) journals
    (debit, credit, (debit - credit))

let outputConsolidationTxt consolidated = 
    printfn "ACCOUNT         |DESCRIPTION         |DEBIT          |CREDIT         |BALANCE        |"
    printfn "________________|____________________|_______________|_______________|_______________|"
    let outputSingle single = 
        let account, journals = single
        let debit, credit, balance = journals |> sumJournals
        printfn "%-16d|%-20s|%-15d|%-15d|%-15d|" account.account account.label debit credit balance
        printfn "----------------|--------------------|---------------|---------------|---------------|"
    List.iter (fun x -> (outputSingle x)) consolidated

let outputConsolidationCsv consolidated = 
    printfn "ACCOUNT,DESCRIPTION,DEBIT,CREDIT,BALANCE"
    let outputSingle single = 
        let account, journals = single
        let debit, credit, balance = journals |> sumJournals
        printfn "%d,%s,%d,%d,%d" account.account account.label debit credit balance
    List.iter (fun x -> (outputSingle x)) consolidated

let testargs = [| ""; "*"; "*"; "*"; "*"; "TEXT" |]

let main (argv : string array) = 
    let firstAccount = 
        match argv.[1] with
        | "*" -> 0
        | _ -> (argv.[1] |> int)
    
    let secondAccount = 
        match argv.[2] with
        | "*" -> 9999
        | _ -> (argv.[2] |> int)
    
    let firstPeriod = 
        match argv.[3] with
        | "*" -> "JAN-01"
        | _ -> argv.[3]
        |> StringToPeriod
    
    let secondPeriod = 
        match argv.[4] with
        | "*" -> "DEC-31"
        | _ -> argv.[4]
        |> StringToPeriod
    
    let _, _, balance = sumJournals Journals
    match balance with
    | 0 -> balance
    | _ -> failwith "Input journal file does not balance"
    |> ignore
    let sorted = 
        Journals
        |> List.filter (fun x -> secondAccount >= x.account)
        |> List.filter (fun x -> firstAccount <= x.account)
        |> List.filter (fun x -> FilterPeriod (x.period |> StringToPeriod) firstPeriod)
        |> List.filter (fun x -> FilterPeriod secondPeriod (x.period |> StringToPeriod))
    
    let consolidated = consolidateAll sorted Accounts
    match argv.[5] with
    | "TEXT" -> outputConsolidationTxt consolidated
    | "CSV" -> outputConsolidationCsv consolidated
    | _ -> failwith "Invalid output type"
    0