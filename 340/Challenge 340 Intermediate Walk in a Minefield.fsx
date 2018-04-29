open System

type Action = North | East | South | West | On | Off | NoAction
    with
        member this.AdjustAmount =
                match this with
                | North -> (0,-1)
                | East -> (1,0)
                | South -> (0,1)
                | West -> (-1,0)
                | _ -> (0,0)
                
        static member FromLetter l =
                match l with
                | 'N' -> North
                | 'E' -> East
                | 'S' -> South
                | 'O' -> West
                | 'I' -> On
                | '-' -> Off
                | _ -> NoAction

        static member FromString (s:string) =
                    s.ToCharArray() |> Array.map Action.FromLetter

type ActionResult<'a> = 
    | Success of 'a
    | Boom of 'a

type Robot = {x:int
              y:int}

type Maze = {maze:char[][]
             height:int
             width:int}
    with
        static member FromString (maze:string) =
               maze.Split('\n')
               |> Array.map (fun s -> s.ToCharArray())
        
        member this.ToMazeString() =
            this.maze
            |> Array.map (fun sub -> 
                sub 
                |> Array.append [|'\n'|]
                |> Array.map (fun c -> c.ToString())
                |> Array.reduce (+))
            |> Array.reduce (+)
        member this.LocatePlayer =
               let found,pos = 
                   this.maze
                   |> Array.fold (fun acc sub -> 
                      let found,pos = acc
                      let _,y = pos
                      if found then 
                          acc
                      else
                          match (sub |> Array.tryFindIndex ((=) 'M')) with
                          | Some index -> (true,(index,y))
                          | None -> (false,(0,y+1))) (false, (0,0))
               let x,y = pos
               if found then Some {x=x;y=y}
               else None

        member this.TrySet (x,y) c =
            if this.CheckValidPosition (x,y) then
               let adjusted = 
                    this.maze
                    |> Array.mapi (fun cY sub->
                         sub
                         |> Array.mapi (fun cX subC ->
                             if cY = y && cX = x then c
                             else subC))
               {this with maze=adjusted}
            else
               this

        member this.TileAt (x,y) =
               this.maze.[y].[x]

        member this.TryGetTileAt (x,y) =
            if this.CheckValidPosition (x,y) then
                Some (this.TileAt (x,y))
            else
                None
                
        member this.CheckValidPosition (x,y) =
            if x >= this.width || x < 0 then false
            else if y >= this.height || y < 0 then false
            else true
            
        member this.IsWinningPosition (x,y) =
            let left =  this.TryGetTileAt (x-1,y)
            let right = this.TryGetTileAt (x+1,y)
            let above = this.TryGetTileAt (x,y-1)
            let below = this.TryGetTileAt (x,y+1)

            let leftRight = (left = Some '+') && (right = Some '+') && (above = None || below = None)
                   
            let aboveBelow = (above = Some '+') && (below = Some '+') && (left = None || right = None)
            
            ((aboveBelow && not leftRight) || (leftRight && not aboveBelow))

let AdjustRobotPosBy (maze:Maze) robot (i,j) =
     let x = robot.x+i
     let y = robot.y+j
     match maze.TryGetTileAt (x,y) with
     | Some '+' -> Success robot
     | Some '*' -> Boom {x=x;y=y}
     | None -> Success robot
     | _ -> Success {x=x; y=y}

let CommandsToMoves commands =
    commands
    |> Array.fold (fun (started,moves) move ->
        match started with
        | true -> 
            match move with
            | Off -> (false,moves)
            | _ -> (true, [move.AdjustAmount] |> List.append moves)
        | false ->
            match move with
            | On -> (true,moves)
            | _ -> (false,moves)) (false,[])

let ApplyMovesToRobot maze robot moves =
    moves 
    |> List.fold (fun (robot,positions) vector ->
        let adjusted = AdjustRobotPosBy maze robot vector
        match adjusted with
        | Success x -> 
            (x,((Success x)::positions))
        | z -> 
            (robot,z::positions)) (robot, [Success robot])

let CreateSteppedMazes (baseMaze:Maze) (robotHistory:ActionResult<Robot> list) =
    robotHistory
    |> List.map (fun action ->
        match action with
        | Success robot -> baseMaze.TrySet (robot.x,robot.y) '&'
        | Boom robot -> baseMaze.TrySet (robot.x,robot.y) '#')
    |> List.rev

let RunCommandOnMaze inputMaze command =
    let commands = command |> Action.FromString
    let converted = inputMaze |> Maze.FromString
    let mazeFromString = Maze.FromString inputMaze
    let maze = {maze=mazeFromString;height=converted.Length;width=converted.[0].Length}
    match maze.LocatePlayer with
    | Some robot ->
        let robotRunning, moves = CommandsToMoves commands

        if robotRunning then 
            printfn "Failed! Robot was still on at end of move chain."
        else
            let finalPosition, moveResults = ApplyMovesToRobot maze robot moves
            if moveResults |> List.exists (fun move -> match move with | Boom _ -> true | _ -> false) then
                printfn "Robot moved into a mine! Boom."
            else
                if finalPosition = robot then
                    printfn "Robot never moved or returned to start or never turned on!"
                else
                    match maze.IsWinningPosition (finalPosition.x,finalPosition.y) with
                    | true -> printfn "Success!"
                    | false -> printfn "Robot got lost! :("
            moveResults
            |> CreateSteppedMazes maze
            |> List.iteri (fun i m -> 
                printf "%d" i
                printfn "%s" (m.ToMazeString()))
            ()
    | None -> printfn "No Robot!"

let test() =
    let testMaze =
        "+++\n\
         M00\n\
         +++\n"
    RunCommandOnMaze testMaze "IEOEE-"
    RunCommandOnMaze testMaze "IOEEE-"
    RunCommandOnMaze testMaze "IEEEO-"
    RunCommandOnMaze testMaze "IOEE-"
    RunCommandOnMaze testMaze "IEEE-"

    let testMaze = 
        "+++++++++++++\n\
         +000000000000\n\
         +0000000*000+\n\
         +00000000000+\n\
         +00000000*00+\n\
         +00000000000+\n\
         M00000000000+\n\
         +++++++++++++\n"
    //RunCommandOnMaze testMaze "ENENNNNEEEEEEEE-" //fail, never moved
    //RunCommandOnMaze testMaze "IENENNNNEEEEEEEE-" //fail, didn't make it
    //RunCommandOnMaze testMaze "IEEEEEEEEENN-" //fail, boom
    //RunCommandOnMaze testMaze "IENENNNNEEEEEEEEEE" //fail, was on at end
    //RunCommandOnMaze testMaze "IOENENNNNEEEEEEEEEE-" //win
    //RunCommandOnMaze testMaze "IENENNNNEEEEEEEEEE-" //win
    //RunCommandOnMaze testMaze "IENENNNNNNNNNNNNNNNEEEEEEEEEE-" //win

    ()