// Learn more about F# at http://fsharp.org

open System
open GraphicsGaleWrapper
open System.IO
open System.Drawing

// serialize a bitmap as png
let toFile (name : string) (bmp: Bitmap) =
    bmp.Save(name, Imaging.ImageFormat.Png) |> ignore
    bmp

let showInfo (go:GaleObject) =
    let width = go.Width
    let height = go.Height
    let totalFrames = go.FrameCount
    printfn "Loaded %ix%i animation with %i frames" width height totalFrames
    ()

[<EntryPoint>]
let main argv =

    printfn "GalSheet v0.1"
    
    try
        let path =
            match (List.ofArray argv) with
                | path::_ -> path
                | _ -> failwith "A path is required"

        let go = new GaleObject(path)
        showInfo go
        let bmp = go.Frames.[0].CreateBitmap()
        let name = path + ".png"
        toFile name bmp |> ignore
        0
    with
        | e ->  printfn "Error: %s" e.Message; 1