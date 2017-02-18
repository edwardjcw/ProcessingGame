open ProcessingGame
open System

[<EntryPoint>]
let main argv = 

    printfn "%s" GameInterface.intro
    
    GameInterface.Play (Console.ReadLine) (Success EnvironmentBuilder.SampleEnv) |> ignore
    Console.ReadKey() |> ignore
    0 // return an integer exit code
