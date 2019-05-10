# GaleSheet

A tool to convert a [Graphics Gale](https://graphicsgale.com/us/) animation to a sprite sheet.

## Usage

```
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
```

This tool will take a number of `.gal` (as many as the glob pattern matches), and generate
a single `.png` sprite sheet, with each `.gal` animation being an horizontal stripe.

It will also make the `.gal` animation backgroud color transparent.

In `--width=AUTO` mode, the sprite sheet will be made as wide as the longest animation strip.

> At the moment, no other file is generated, so data such as timing, and frame coordinates, indexes, filenames, are not exported.

## Build requirements

This tool makes use of the (oficial) library provided by HumanBalance.

To compile this project, you must first [download](https://graphicsgale.com/us/download.html) the library, unzip and put it in the `galefile/` folder.

> This project is tested to work with the latest available library version, which at this time is `ver.15.11.19`

You will also need the **.NET** SDK, grab it from [here](https://dotnet.microsoft.com/learn/dotnet/hello-world-tutorial).

## Using `galesheet`

The program accepts multiple `.gal` files, in the **same** format.

- 24 bits per pixel (full color)
- 8 bits per pixel (palette)

In both cases, the output is 32 bpp `.png` file.
In the former, the background color will be made **transparent**.
In the latter, the palette **index** will be the stored in the BLUE component.

> The idea is that you'll use some kind of pixel shader to adjust the pixels to the right color

## QA

Q: Okay, got it compiled. How do I use it?

A: This is how I use it.
 It might not be the best way, my Windows knowlegde is limited.

1. Go to the `galesheet\bin\Debug\net461` directory
1. Right click on `galesheet.exe` and `Create shortcut`
1. Move the shortcut `.lnk` to somewhere so its in the Windows %PATH%
1. Create a `.cmd` file in a folder containing your animations like this one:
    ```cmd
    galesheet.exe.lnk --destination="%CD%\spritesheet.png" "%CD%\*.gal"
    ```
1. Double click on it to begin the process

Q: How does `--flip` work? Can I flip in both axis?

A: The flip applied to each and every frame.

 Add one or two flips separately, like this  (order does not matter): 
 ```
 galesheet.exe --flip=H --flip=V *.gal
 ```

## Acknowledgements

This tool also uses a C# wrapper to the `dll` provided by [SebaGames](https://twitter.com/sebagamesdev).

## Contributing

This tool has been created for a very particular, personal need, so it might not fit other use cases. Feature requests are **discouraged**.  

You're free and encouraged to fork and adapt it to your needs, though.

Bug reports and/or fixes are **welcome**.