# GaleSheet

A tool to convert a [Graphics Gale](https://graphicsgale.com/us/) animation to a sprite sheet.

## Usage

```
GaleSheet: 
    A tool to create sprite sheets from Graphics Gale .gal files

Usage: 
    galesheet [--width=<width>] [--destination=<filename>] <path>

Options:
    --version                Show version.
    --width=<width>          Set sheet width [default: AUTO].
    --destination=<filename> Destination filename [default: spritesheet.png]
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

## QA

Q: Okay, got it compiled. How do I use it?

A: This is how I use it.
 It might not be the best way, my Windows knowlegde is limited.

1. Go to the `galesheet\bin\Debug\net461` directory
1. Right click on `galesheet.exe` and `Create shortcut`
1. Move the shortcut `.lnk` to somewhere so its in the Windows %PATH%
1. Create a `.bat` file in a folder containing your animations like this one:
    ```bat
    galesheet.exe.lnk --destination="%CD%\spritesheet.png" "%CD%\*.gal"
    ```
1. Double click on it to begin the process

## Acknowledgements

This tool also uses a C# wrapper to the `dll` provided by [SebaGames](https://twitter.com/sebagamesdev).

## Contributing

This tool has been created for a very particular, personal need, so it might not fit other use cases. Feature requests are **discouraged**.  

You're free and encouraged to fork and adapt it to your needs, though.

Bug reports and/or fixes are **welcome**.