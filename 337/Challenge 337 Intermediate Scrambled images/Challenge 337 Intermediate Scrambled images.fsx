open System.Drawing
let LoadImage(file : string) = new Bitmap(file)

let LoadBitmapColors(bmp : Bitmap) = 
    [ for y in 0..bmp.Height-1 -> 
           [ for x in 0..bmp.Width-1 -> bmp.GetPixel(x, y) ] ]

//will crash if amount > list, works for my purposes though
let ShiftListRight (amount:int) (list:'a List) =
    // 0 1 2
    // 3 4 5
    // 6 7 8
    let endChunk = list.[(list.Length-amount)..list.Length-1]
    let shifted = endChunk.[0..amount-1] @ list.[0..(list.Length-amount-1)]
    shifted

//takes about 20+ seconds per image because Bitmap.SetPixel is crazy slow and I don't feel like building an entire helper method for this
let ToBitmap (width:int) (lines:Color list) =
    let height = lines.Length/width
    let bmp = new Bitmap(width,height)

    for y in 0..height-1 do
        for x in 0..width-1 do
            bmp.SetPixel(x,y,lines.[(y*width+x)])

    bmp

let LocateColor (color:Color) (list:Color list) =
    (list |> List.findIndex ((=) color))

// count # of color in single stretch
let CountContColorAtPos (searchColor:Color) (list:Color list) (pos:int) =
    list
    |> List.splitAt (pos)
    |> snd //get latter half of list where the colors are
    |> List.fold (fun (count,skip) color -> 
            if skip then 
                (count,skip)
            else
                if color = searchColor then 
                    ((count+1),false) 
                else 
                    (count,true)) (0,false)
    |> fst //get count, we don't care about the skip variable


let unscrambleImage (searchColor:Color) (filename:string) (outputName:string) =
    let unscramble (width:int) (lines: Color list list) =
        lines
        |> List.map (fun line -> 
                let pos = LocateColor searchColor line
                let count = CountContColorAtPos searchColor line pos
                let shiftAmount = width - pos - count
                ShiftListRight shiftAmount line)

    let colors = 
        LoadImage filename
        |> LoadBitmapColors

    let width = colors.[0].Length

    let bmp = 
        unscramble width colors
        |> List.collect id
        |> ToBitmap width

    bmp.Save(outputName)

[<EntryPoint>]
let main argv = 
    let searchColor = Color.FromArgb(255,0,0)
    let inputs  = [|"1.png";"2.png";"3.png"|]
    let outputs = [|"u1.png";"u2.png";"u3.png"|]
    Array.iter2 (fun file output -> 
          printfn "Unscrambling %s to %s"  file output
          unscrambleImage searchColor file output
          printfn "Unscrambled %s to %s" file output) inputs outputs
    printfn "Done."
    0 // return an integer exit code