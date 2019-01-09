// Learn more about F# at http://fsharp.org

open GraphicsGaleWrapper
open System.Drawing
open System.Drawing.Drawing2D

open Fake.IO.Globbing.Operators
open Fake.Core
open System.IO

[<Literal>]
let VERSION = "0.1"

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

let blitFrame (sheet:Graphics) (x:int) (frame:Frame) =
    let bmp = frame.CreateBitmap()
    sheet.DrawImage(bmp, x, 0, frame.Width, frame.Height) |> ignore
    x + frame.Width

[<EntryPoint>]
let main argv =

    //  command line uses http://docopt.org/

    let doc = """
GaleSheet: 
    A tool to create sprite sheets from Graphics Gale .gal files

Usage: 
    galesheet <path>

Options:
    --version     Show version.
"""

    try
        let parser = Docopt(doc)
        let parsedArguments = parser.Parse(argv)

        let path = 
            match parsedArguments.TryFind "<path>" with
                | Some (DocoptResult.Argument path) -> path
                | _ -> failwith "A path (or glob) is required"
                
        let animations = !! path

        for file in animations do

            printfn "File path: %s" file

            let go = new GaleObject(file)

            showInfo go

            let sheet = new Bitmap(go.Width*go.FrameCount, go.Height)
    
            let gsheet = Graphics.FromImage(sheet)
            gsheet.CompositingMode <- CompositingMode.SourceCopy
            
            go.Frames 
                |> Array.fold (blitFrame gsheet) 0
                |> ignore
            
            let name = file + ".sheet.png"

            toFile name sheet |> ignore
        
        0
    
    with
        | e ->  printfn "%s" e.Message; printfn "%s" doc; 1