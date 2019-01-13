// Learn more about F# at http://fsharp.org

open GraphicsGaleWrapper
open System.Drawing

open Fake.IO.Globbing.Operators
open Fake.Core

[<Literal>]
let VERSION = "0.1"

[<Literal>]
let DOC = """
GaleSheet: 
    A tool to create sprite sheets from Graphics Gale .gal files

Usage: 
    galesheet [--width=<width>] [--destination=<filename>] <path>

Options:
    --version                Show version.
    --width=<width>          Set sheet width [default: AUTO].
    --destination<filename>  Destination filename [default: spritesheet.png]
"""

[<Literal>]
let DEFAULT_DESTINATION = "spritesheet.png"

type Width = 
    | Auto
    | Fixed of int

type Destination = 
    | Auto
    | Filename of string

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

let blit (source:Bitmap) (destination:Bitmap) (position:Point) =
    if position.X + source.Width > destination.Width || position.Y + source.Height > destination.Height
        then 
            Error "Unable to blit! Out of bounds"
        else 

            for x in 0..(source.Width-1) do
                for y in 0..(source.Height-1) do
                    destination.SetPixel(position.X + x, position.Y + y, source.GetPixel(x,y))

            Ok destination

let blitFrame (sheet:Bitmap) (x:int) (frame:Frame) =
    let bmp = frame.CreateBitmap()

    match (blit bmp sheet (new Point(x,0))) with
        | Error e -> printfn "%A" e
        | Ok _ -> ()

    x + frame.Width

[<EntryPoint>]
let main argv =

    //  command line uses http://docopt.org/

    let command = Docopt(DOC)

    try
        
        let parsedArguments = command.Parse(argv)

        let path = 
            match DocoptResult.tryGetArgument "<path>" parsedArguments with
                | Some path -> path
                | _ -> failwith "A path (or glob) is required"
                
        let width =  
            match DocoptResult.tryGetArgument "--width" parsedArguments with
                | Some "AUTO" | None -> Width.Auto
                | Some w -> Width.Fixed (int w)

        let destination =
            match DocoptResult.tryGetArgument "--destination" parsedArguments with
                | None -> failwith "Unexpected error: Destination filename should have a default"
                | Some filename -> filename

        printfn "Arguments: %A" parsedArguments
        

        let animations = !! path

        for file in animations do

            printfn "File path: %s" file

            let go = new GaleObject(file)

            showInfo go

            let sheet = new Bitmap(go.Width*go.FrameCount, go.Height)
            
            go.Frames 
                |> Array.fold (blitFrame sheet) 0
                |> ignore

            toFile destination sheet |> ignore
        
        0
    
    with
        | e ->  printfn "%s" e.Message; printfn "%s" DOC; 1