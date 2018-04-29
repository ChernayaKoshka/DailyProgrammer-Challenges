open System
open System.IO

type PowerNode = {NodeId:int; Consumption:float; Generation:float}

type NodeConnection = {NodeId:int; Connections:int[]}

module Array =
    let collapse(kv:('a*'b)[]) =
        let data1, _ = kv |> Array.unzip
        let keys = data1 |> Array.distinct
        keys
        |> Array.map (fun x -> (x, kv 
                                   |> Array.filter (fun (k,_) -> k=x) 
                                   |> Array.map snd))

let Openfile filename =
    File.ReadAllLines(filename)

let ParseNodeList (data:string[]) =
    //skip first line, since we don't need to know the # of nodes'
    data.[1..]
    |> Array.map (fun item ->
          let split = item.Split [|' '|]
          {
            NodeId = split.[0] |> int
            Consumption = split.[1]|>float
            Generation = split.[2] |> float
          })

let ParseNodesFromFile = Openfile >> ParseNodeList

let ParseEdges (data:string[]) =
    data.[1..] //skip number of edges, we don't need it 
    |> Array.map (fun x -> 
             let split = x.Split [|' '|]
             (split.[0]|>int,split.[1]|>int))
    |> Array.collapse
    |> Array.map (fun (node,connections) -> {NodeId=node;Connections=connections})
    