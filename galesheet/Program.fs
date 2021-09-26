// Learn more about F# at http://fsharp.org

open GraphicsGaleWrapper
open System.Drawing

open Fake.IO.Globbing.Operators
open Fake.Core

[<Literal>]
let VERSION = "0.4"

[<Literal>]
let DOC = """
GaleSheet: 
    A tool to create sprite sheets from Graphics Gale .gal files

Usage:
    galesheet [--version]
    galesheet [--width=<width>] [--destination=<filename>] [--palette=<colors>] ([--flip=<flip>] ...) <path>

Options:
    --version                 Show version.
    --flip=<flip>             Flips frames, H orizontal and/or V ertically [default: none].
    --width=<width>           Set sheet width [default: AUTO].
    --palette=<colors>        Save "palette" data of the given size [default: none].
    --destination=<filename>  Destination filename [default: spritesheet.png].
    --channel=<channel>       Choose the channel (R,G, or B) to store the pixel palette index [default: R].
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

type Channel =
    | R
    | G
    | B

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

    let makeOpaque (c:Color) : Color = Color.FromArgb (255, c)

open BitColor
open System
open System.Drawing.Imaging
open Microsoft.FSharp.NativeInterop

// serialize a bitmap as png
let toFile (name : string) (bmp: Bitmap) =
    bmp.Save(name, ImageFormat.Png) |> ignore
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

let colorWithPaletteIndexInChannel channel v (c:Color) =
    match channel with
    | R -> Color.FromArgb(int v, int c.G, int c.B)
    | G -> Color.FromArgb(int c.R, int v, int c.B)
    | B -> Color.FromArgb(int c.R, int c.G, int v)

let tryConvertBitmapToRGB (c:Channel) (originalBmp:Bitmap): Bitmap =

    match originalBmp.PixelFormat with
    | PixelFormat.Format32bppArgb | PixelFormat.Format24bppRgb -> originalBmp
    | PixelFormat.Format8bppIndexed ->

        // map data to an array. We can't use GetPixel() because it 
        // returns a color, not the palette index value :(

        let convertedBmp = new Bitmap(originalBmp.Width, originalBmp.Height, PixelFormat.Format24bppRgb)
        let bounds = Rectangle(0,0,originalBmp.Width,originalBmp.Height)
        let maxY = bounds.Bottom-1
        let bmpData = originalBmp.LockBits(bounds, ImageLockMode.ReadOnly, originalBmp.PixelFormat)

        // If stride is negative, is because the image is upside down!
        // It also means that Scan0 gives you the addr of the last row!
        // If we want to marshall copy we must find the actual start of
        // the data, which is h-1 * stride before the scan0
        // See thread: https://stackoverflow.com/questions/6835006/how-can-i-copy-the-pixel-data-from-a-bitmap-with-negative-stride
        
        let addr = bmpData.Scan0
        let mutable ptr = NativePtr.ofNativeInt<uint8> addr
        ptr <- NativePtr.add ptr ((originalBmp.Height-1)*bmpData.Stride)
        
        let stride = abs(bmpData.Stride)
        let bytes = stride * bmpData.Height
        let values:uint8[] =  Array.zeroCreate<uint8> bytes
        System.Runtime.InteropServices.Marshal.Copy(ptr |> NativePtr.toNativeInt , values, 0, bytes);
        originalBmp.UnlockBits bmpData
        
        let originalPalette = originalBmp.Palette.Entries
        let palette = originalPalette |> Array.mapi (colorWithPaletteIndexInChannel c)

        values |> Array.iteri (fun i v -> 
            // remember, the rows in values are from bottom to top
            let x = i % stride
            let y = maxY - (i / stride)
            let c = palette.[int v] 

            try 
                convertedBmp.SetPixel(x,y,c)
            with 
                e -> printfn "SetPixel failed for: %i %i" x y |> ignore
        ) |> ignore

        convertedBmp

    | unsupported -> failwithf "Unsupported PixelFormat %A" unsupported

let blit (source:BlitSource) (destination:BlitDestination) =
    
    let left = destination.Position.X
    let top = destination.Position.Y
    let maxX = source.Bitmap.Width-1
    let maxY = source.Bitmap.Height-1
    let bgcolor = source.BgColor
    let sourceBmp = source.Bitmap
    let destBmp = destination.Bitmap

    let inline skipPixel p = bgcolor.IsSome && bgcolor.Value.Equals p

    for x in 0..maxX do
        for y in 0..maxY do
            let pixel = sourceBmp.GetPixel(x, y)
            if not (skipPixel pixel) then destBmp.SetPixel(left + x, top + y, pixel) |> ignore

let tryBlit (source:BlitSource) (destination:BlitDestination) =
    match source.Bitmap.Size with
    | InBounds destination.Bitmap.Size destination.Position -> 
        blit source destination
        Ok destination
    | _  ->
        let error = Error "Unable to blit! Out of bounds"
        printfn "%A" error
        error

let blitFrame (sheet:Bitmap) (bgcolor:Color) (flipValue:RotateFlipType) (c:Channel) (x:int) (frame:Frame) =
    let sourceBmp = frame.CreateBitmap() |> (tryConvertBitmapToRGB c)
    // NOTICE: We want to preserve frame order
    // That's why we flip frame by frame
    sourceBmp.RotateFlip flipValue |> ignore
    let source = BlitSource (sourceBmp, Some bgcolor)
    let destination = BlitDestination (sheet, Point(x, 0))
    tryBlit source destination |> ignore
    x + frame.Width

let blitStrip (sheet:Bitmap) (y:int) (strip:Bitmap) =
    let source = BlitSource (strip, None)
    let destination = BlitDestination (sheet, Point(0, y))
    tryBlit source destination |> ignore
    y + strip.Height

let solidColorBitmap (c:Color) (b:Bitmap) =
    for x in 0..(b.Size.Width - 1) do
        for y in 0..(b.Size.Height - 1) do
            b.SetPixel(x,y,c)
    b

[<EntryPoint>]
let main argv =

    //  command line uses http://docopt.org/

    let command = Docopt(DOC)

    try
        
        let parsedArguments = command.Parse(argv)
        printfn "Arguments: %A" parsedArguments

        let showVersion =  DocoptResult.hasFlag "--version" parsedArguments
        
        if showVersion then failwith (sprintf "%s" VERSION)

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

        let flipValue =
            match DocoptResult.tryGetArguments "--flip" parsedArguments with
            | Some [ "H" ] -> RotateFlipType.RotateNoneFlipX
            | Some [ "V" ] -> RotateFlipType.RotateNoneFlipY
            | Some [ "H"; "V"] | Some [ "V"; "H"] -> RotateFlipType.RotateNoneFlipXY
            | _ -> RotateFlipType.RotateNoneFlipNone
         
        // Begin animation files processing

        let channelValue = 
            match DocoptResult.tryGetArgument "--channel" parsedArguments with
            | Some "R" -> R
            | Some "G" -> G
            | Some "B" -> B
            | None
            | _ -> failwith "Unexpected error: Channel option should have a default"

        let files = !! pathValue

        let mutable strips = List.empty
        let mutable palette = Array.zeroCreate<Color> 0
        let mutable bgColor = Color.Transparent

        for file in Seq.rev files do

            printfn "File path: %s" file

            use go = new GaleObject(file)

            showInfo go

            let strip = new Bitmap(go.Width*go.FrameCount, go.Height)
            printfn "%A" strip.Size
            
            bgColor <- go.BackgroundColor |> Color.FromArgb |> makeOpaque

            // convert frames to strips

            go.Frames 
                |> Array.fold (blitFrame strip bgColor flipValue channelValue) 0
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
        
        let convertedBGColor = colorWithPaletteIndexInChannel channelValue 0 bgColor
        let sheet = (solidColorBitmap convertedBGColor) <| new Bitmap(sheetWidth, sheetHeight, PixelFormat.Format24bppRgb)

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
    | e ->  printfn "%s" e.Message; printfn "%s" e.StackTrace; printfn "%s" DOC; 1
