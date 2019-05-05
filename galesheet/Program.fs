// Learn more about F# at http://fsharp.org

open GraphicsGaleWrapper
open System.Drawing

open Fake.IO.Globbing.Operators
open Fake.Core

[<Literal>]
let VERSION = "0.2"

[<Literal>]
let DOC = """
GaleSheet: 
    A tool to create sprite sheets from Graphics Gale .gal files

Usage: 
    galesheet [--width=<width>] [--destination=<filename>] [--palette=<colors>] <path>

Options:
    --version                 Show version.
    --width=<width>           Set sheet width [default: AUTO].
    --palette=<colors>        Save "palette" data of the given size [default: none].
    --destination=<filename>  Destination filename [default: spritesheet.png].
"""

[<Literal>]
let DEFAULT_DESTINATION = "spritesheet.png"

type Width = 
    | Auto
    | Fixed of int

type Destination = 
    | Auto
    | Filename of string

type ColorFormat =
    | FullColor
    | Palette of uint8

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
        [ c.A; c.R; c.G; c.B; ]
        |> List.map byte2Hex
        |> List.reduce (+)

    let makeOpaque (c:Color) : Color = Color.FromArgb (c.ToArgb() ||| 0xFF000000)

open BitColor
open System
open System.Drawing.Imaging
open Microsoft.FSharp.NativeInterop

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

type BlitSource = 
    | BlitSource of Bitmap * Color option
    
    member self.BgColor = 
        match self with
        | BlitSource (_,bgcolor) -> bgcolor

    member self.Bitmap = 
        match self with
        | BlitSource (bitmap,_) -> bitmap

type BlitDestination = 
    | BlitDestination of Bitmap * Point
    
    member self.Position = 
        match self with
        | BlitDestination (_,point) -> point

    member self.Bitmap = 
        match self with
        | BlitDestination (bitmap,_) -> bitmap

let blit (source:BlitSource) (destination:BlitDestination) =
    
    let left = destination.Position.X
    let top = destination.Position.Y
    let maxX = source.Bitmap.Width-1
    let maxY = source.Bitmap.Height-1
    let bgcolor = source.BgColor
    let sourceBmp = source.Bitmap
    let destBmp = destination.Bitmap

    let inline skipPixel p = bgcolor.IsSome && bgcolor.Value.Equals p

    printfn "Pixel Format:%A" source.Bitmap.PixelFormat

    match source.Bitmap.PixelFormat with
        | PixelFormat.Format32bppArgb | PixelFormat.Format24bppRgb ->
   
            for x in 0..maxX do
                for y in 0..maxY do 
                    
                    let pixel = sourceBmp.GetPixel(x, y)
                    if skipPixel pixel then () else destBmp.SetPixel(left + x, top + y, pixel)

        | PixelFormat.Format8bppIndexed ->
            let palette = sourceBmp.Palette.Entries
            // let bounds = sourceBmp.GetBounds(ref GraphicsUnit.Pixel)
            let bounds = Rectangle(0,0,sourceBmp.Width,sourceBmp.Height)
            let bmpData = sourceBmp.LockBits(bounds, ImageLockMode.ReadOnly, source.Bitmap.PixelFormat)
            // If stride is negative, is because the image is upside down!
            // It also means that Scan0 gives you the addr of the last row!
            // If we want to marshall copy we must find the actual start of
            // the data, which is h-1 * stride before the scan0
            // See thread: https://stackoverflow.com/questions/6835006/how-can-i-copy-the-pixel-data-from-a-bitmap-with-negative-stride
            let addr = bmpData.Scan0
            let mutable ptr = NativePtr.ofNativeInt<uint8> addr
            ptr <- NativePtr.add ptr ((sourceBmp.Height-1)*bmpData.Stride)
            
            let stride = abs(bmpData.Stride)
            printfn "Stride:%A" stride
            let bytes = (stride * bmpData.Height)
            // Declare an array to hold the bytes of the bitmap.
            let values:uint8[] =  Array.zeroCreate<uint8> bytes

            // Copy the RGB values into the array.
            System.Runtime.InteropServices.Marshal.Copy(ptr |> NativePtr.toNativeInt , values, 0, bytes);

            sourceBmp.UnlockBits bmpData

            values |> Array.iteri (fun i v -> 
                // remember that the rows in values are from bottom to top
                let x = left + i % stride
                let y = top + maxY - (i / stride)
                // read actual from palette, set BLUE component to the palette index
                let c = Color.FromArgb(int palette.[int v].R, int palette.[int v].G, int v)
                destBmp.SetPixel(x,y,c)
            ) |> ignore
        | unsupported -> failwithf "Unsupported PixelFormat %A" unsupported

let tryBlit (source:BlitSource) (destination:BlitDestination) =
    match source.Bitmap.Size with
    | InBounds destination.Bitmap.Size destination.Position -> 
        blit source destination
        Ok destination
    | _  ->
        let error = Error "Unable to blit! Out of bounds"
        printfn "%A" error
        error

let blitFrame (sheet:Bitmap) (bgcolor:Color) (x:int) (frame:Frame) =
    let source = BlitSource (frame.CreateBitmap(), Some bgcolor)
    let destination = BlitDestination (sheet, Point(x, 0))
    tryBlit source destination |> ignore
    x + frame.Width

let blitStrip (sheet:Bitmap) (y:int) (strip:Bitmap) =
    let source = BlitSource (strip, None)
    let destination = BlitDestination (sheet, Point(0, y))
    tryBlit source destination |> ignore
    y + strip.Height

[<EntryPoint>]
let main argv =

    //  command line uses http://docopt.org/

    let command = Docopt(DOC)

    try
        
        let parsedArguments = command.Parse(argv)
        printfn "Arguments: %A" parsedArguments

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

        let paletteValue =
            match DocoptResult.tryGetArgument "--palette" parsedArguments with
            | None -> failwith "Unexpected error: Palette option should have a default"
            | Some "none" -> FullColor
            | Some value -> 
                match Byte.TryParse(value) with
                | true, v    -> Palette v
                | false, _   -> failwith "Argument error! Palette should be: 1 <= colors <= 256"

        let files = !! pathValue

        let mutable strips = List.empty
        let mutable palette = Array.zeroCreate<Color> 0

        for file in Seq.rev files do

            printfn "File path: %s" file

            use go = new GaleObject(file)

            showInfo go

            let strip = new Bitmap(go.Width*go.FrameCount, go.Height)
            printfn "%A" strip.Size
            
            let bgColor = go.BackgroundColor |> Color.FromArgb |> makeOpaque

            // convert frames to strips

            go.Frames 
                |> Array.fold (blitFrame strip bgColor) 0
                |> ignore

            strips <- strip::strips

            // save palette, if needed
            
            let frame = go.Frames.[0]

            match (Array.isEmpty palette, paletteValue) with
            | (true, ColorFormat.Palette size) ->
                if not go.SinglePalette then failwith "Palette animations must have a single palette!"
                try palette <- (go.Palette.Entries |> Array.take (int size))
                with | e ->  failwith "Unable to extract palette!"
                ()
            | _ -> ()
            |> ignore

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

        // print palette

        if not (Array.isEmpty palette) then
            printfn "Palette:"
            palette |>  Array.map color2hex |> Array.iteri (fun i v -> printfn "%i : %s" i v)

        // save palette
        
        // TODO

        // all done!
        0
    
    with
    | e ->  printfn "%s" e.Message; printfn "%s" DOC; 1
