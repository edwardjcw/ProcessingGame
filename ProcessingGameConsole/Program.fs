open ProcessingGame
open System

[<EntryPoint>]
let main argv = 

    GamePlay.Start |> Seq.last |> ignore
    Console.ReadKey() |> ignore
    0 // return an integer exit code
