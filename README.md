# GaleSheet

A tool to convert a [Graphics Gale](https://graphicsgale.com/us/) animation to an sprite sheet.

## Usage

```
GaleSheet: 
    A tool to create sprite sheets from Graphics Gale .gal files

Usage: 
    galesheet [--width=<width>] [--destination=<filename>] <path>

Options:
    --version                Show version.
    --width=<width>          Set sheet width [default: AUTO].
    --destination<filename>  Destination filename [default: spritesheet.png]
```

## Build requirements

This tool makes use of the (oficial) library provided by HumanBalance.

To compile this project, you must first [download](https://graphicsgale.com/us/download.html) the library, unzip and put it in the `galefile/` folder.

> This project is tested to work with the latest available library version, which at this time is `ver.15.11.19`

You will also need the **.NET** SDK, grab it from [here](https://dotnet.microsoft.com/learn/dotnet/hello-world-tutorial).


## Acknowledgements

This tool also uses a C# wrapper to the `dll` provided by [SebaGames](https://twitter.com/sebagamesdev).