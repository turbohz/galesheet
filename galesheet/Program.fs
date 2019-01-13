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
            let error = Error "Unable to blit! Out of bounds"
            printfn "%A" error
            error
            
        else 

            for x in 0..(source.Width-1) do
                for y in 0..(source.Height-1) do
                    destination.SetPixel(position.X + x, position.Y + y, source.GetPixel(x,y))

            Ok destination

let blitFrame (sheet:Bitmap) (x:int) (frame:Frame) =
    let bmp = frame.CreateBitmap()
    blit bmp sheet (new Point(x,0)) |> ignore
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
        

        let files = !! path

        let mutable strips = List.empty
        let mutable max = 0
        let mutable height = 0

        for file in files do

            printfn "File path: %s" file

            use go = new GaleObject(file)

            showInfo go

            let sheet = new Bitmap(go.Width*go.FrameCount, go.Height)
            
            go.Frames 
                |> Array.fold (blitFrame sheet) 0
                |> ignore

            strips <- sheet::strips

            if sheet.Width > max then max <- sheet.Width
            if go.Height > height then height <- go.Height

        if strips.IsEmpty then failwith "No files to process!"

        let collage = new Bitmap(max, strips.Length*height)

        strips
            |> List.rev
            |> List.iteri ( fun i b -> blit b collage (new Point(0,i*height)) |> ignore )
            |> ignore

        toFile destination collage |> ignore

        // all done!
        0
    
    with
        | e ->  printfn "%s" e.Message; printfn "%s" DOC; 1