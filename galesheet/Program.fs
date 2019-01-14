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

let (|InBounds|_|) (inner:Size) (position:Point) (outer:Size) = 
    if position.X + inner.Width > outer.Width || position.Y + inner.Height > outer.Height then Some () else None    

let blit (source:Bitmap) (destination:Bitmap) (position:Point) =
    match source.Size with
        | InBounds destination.Size position -> 
            for x in 0..(source.Width-1) do
                for y in 0..(source.Height-1) do
                    destination.SetPixel(position.X + x, position.Y + y, source.GetPixel(x,y))

            Ok destination
        | _  ->
            let error = Error "Unable to blit! Out of bounds"
            printfn "%A" error
            error            

let blitFrame (sheet:Bitmap) (x:int) (frame:Frame) =
    let bmp = frame.CreateBitmap()
    blit bmp sheet (new Point(x,0)) |> ignore
    x + frame.Width

let blitStrip (sheet:Bitmap) (y:int) (strip:Bitmap) =
    blit strip sheet (new Point(0,y)) |> ignore
    y + strip.Height

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
                
        let widthArg =  
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

        for file in files do

            printfn "File path: %s" file

            use go = new GaleObject(file)

            showInfo go

            let strip = new Bitmap(go.Width*go.FrameCount, go.Height)
            
            go.Frames 
                |> Array.fold (blitFrame strip) 0
                |> ignore

            strips <- strip::strips

        if strips.IsEmpty then failwith "No files to process!"

        let sheetWidth = 
            match widthArg with 
                | Width.Auto -> (strips |> List.maxBy (fun s -> s.Width)).Width
                | Width.Fixed w -> w
        
        let sheetHeight = strips |> List.sumBy (fun s -> s.Height)

        printfn "Result sheet is %ix%i" sheetHeight sheetHeight

        let sheet = new Bitmap(sheetWidth, sheetHeight)

        strips
            |> List.rev
            |> List.fold (blitStrip sheet) 0
            |> ignore

        toFile destination sheet |> ignore

        // all done!
        0
    
    with
        | e ->  printfn "%s" e.Message; printfn "%s" DOC; 1