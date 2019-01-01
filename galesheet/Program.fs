// Learn more about F# at http://fsharp.org

open System
open GraphicsGaleWrapper

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let go = new GaleObject("assets/bat_fly.gal")
    printfn "Loaded animation %ix%i" go.Width go.Height
    0 // return an integer exit code
