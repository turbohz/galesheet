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
    --destination=<filename>  Destination filename [default: spritesheet.png]
"""

[<Literal>]
let DEFAULT_DESTINATION = "spritesheet.png"

type Width = 
    | Auto
    | Fixed of int

type Destination = 
    | Auto
    | Filename of string

module BitColor =

    // color conversion and printing functions

    let toHexDigit (ui:uint32) =
        if ui < 10u then char (ui + 0x30u) else char (ui + 0x37u)

    let byte2Hex (b:byte) =
        let ui = uint32 b
        let upper i = i &&& 0xF0u >>> 4 |> toHexDigit |> string
        let lower i = i &&& 0x0Fu >>> 0 |> toHexDigit |> string
        (upper ui) + (lower ui)

    let int2Hex i =
        let ui = uint32 i
        [
            ui &&& 0xFF000000u >>> 24 ;
            ui &&& 0x00FF0000u >>> 16 ;
            ui &&& 0x0000FF00u >>> 08 ;
            ui &&& 0x000000FFu >>> 00 ;
            
        ] |> List.map (byte >> byte2Hex) |> List.reduce (+)

    let color2hex (c:Color) =
        [c.R; c.G; c.B; c.A ]
        |> List.map byte2Hex
        |> List.reduce (+)

    let makeOpaque (c:Color) : Color = Color.FromArgb (c.ToArgb() ||| 0xFF000000)

open BitColor

// serialize a bitmap as png
let toFile (name : string) (bmp: Bitmap) =
    bmp.Save(name, Imaging.ImageFormat.Png) |> ignore
    bmp

let showInfo (go:GaleObject) =
    let width = go.Width
    let height = go.Height
    let totalFrames = go.FrameCount
    let bgColor = go.BackgroundColor |> int2Hex
    printfn "Loaded %ix%i animation with %i frames (Background: #%s)" width height totalFrames bgColor
    ()

let (|InBounds|_|) (outer:Size) (position:Point) (inner:Size) = 
    if position.X + inner.Width <= outer.Width && position.Y + inner.Height <= outer.Height then Some () else None    

let blit (source:Bitmap) (destination:Bitmap) (position:Point) (bgColor:Color option) =
    
    let left = position.X
    let top = position.Y
    let maxX = source.Width-1
    let maxY = source.Height-1

    let skipPixel p = bgColor.IsSome && bgColor.Value.Equals p
    
    for x in 0..maxX do
        for y in 0..maxY do 
            let pixel = source.GetPixel(x, y)
            if skipPixel pixel then () else destination.SetPixel(left + x, top + y, pixel)

let tryBlit (source:Bitmap) (destination:Bitmap) (position:Point) (bgColor:Color option) =
    match source.Size with
    | InBounds destination.Size position -> 
        blit source destination position bgColor
        Ok destination
    | _  ->
        let error = Error "Unable to blit! Out of bounds"
        printfn "%A" error
        error

let blitFrame (sheet:Bitmap) (bgcolor:Color) (x:int) (frame:Frame) =
    let bmp = frame.CreateBitmap()
    tryBlit bmp sheet (Point(x, 0)) (Some bgcolor) |> ignore
    x + frame.Width

let blitStrip (sheet:Bitmap) (y:int) (strip:Bitmap) =
    tryBlit strip sheet (Point(0, y)) None |> ignore
    y + strip.Height

[<EntryPoint>]
let main argv =

    //  command line uses http://docopt.org/

    let command = Docopt(DOC)

    try
        
        let parsedArguments = command.Parse(argv)

        let pathValue = 
            match DocoptResult.tryGetArgument "<path>" parsedArguments with
            | Some path -> path
            | _ -> failwith "A path (or glob) is required"
                
        let widthValue =  
            match DocoptResult.tryGetArgument "--width" parsedArguments with
            | Some "AUTO"
            | None -> Width.Auto
            | Some w -> Width.Fixed (int w)

        let destinationValue =
            match DocoptResult.tryGetArgument "--destination" parsedArguments with
            | None -> failwith "Unexpected error: Destination filename should have a default"
            | Some filename -> filename

        printfn "Arguments: %A" parsedArguments

        let files = !! pathValue

        let mutable strips = List.empty

        for file in Seq.rev files do

            printfn "File path: %s" file

            use go = new GaleObject(file)

            showInfo go

            let strip = new Bitmap(go.Width*go.FrameCount, go.Height)
            printfn "%A" strip.Size
            
            let bgColor = go.BackgroundColor |> Color.FromArgb |> makeOpaque

            go.Frames 
                |> Array.fold (blitFrame strip bgColor) 0
                |> ignore

            strips <- strip::strips

        if strips.IsEmpty then failwith "No files to process!"

        let sheetWidth = 
            match widthValue with 
            | Width.Auto -> (strips |> List.maxBy (fun s -> s.Width)).Width
            | Width.Fixed w -> w
        
        let sheetHeight = strips |> List.sumBy (fun s -> s.Height)

        printfn "Result sheet is %ix%i" sheetWidth sheetHeight

        let sheet = new Bitmap(sheetWidth, sheetHeight)

        strips
            |> List.fold (blitStrip sheet) 0
            |> ignore

        printfn "Saving: %s" destinationValue
        toFile destinationValue sheet |> ignore

        // all done!
        0
    
    with
    | e ->  printfn "%s" e.Message; printfn "%s" DOC; 1
