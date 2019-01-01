// Learn more about F# at http://fsharp.org

open System
open GraphicsGaleWrapper
open System.IO

[<EntryPoint>]
let main argv =
    
    printfn "GalSheet v0.1"

    try 
        let path = 
            match (List.ofArray argv) with
                | path::_ -> path
                | _ -> failwith "Invalid path"

        let go = new GaleObject(path)
        printfn "Loaded animation %ix%i" go.Width go.Height
        0
    with
        | e ->  printfn "Error: %s" e.Message; 1