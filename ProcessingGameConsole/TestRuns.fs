namespace ProcessorGame
open ProcessingGame
open System

type TestRuns() =
    static let processors = 
        (Seq.initInfinite (fun i -> Game.EmptyProcessor 10 1)) 
        |> Seq.take 5
        |> Seq.map (fun p -> (p.id, p)) 
        |> Map.ofSeq

    static let programs = 
        (Seq.initInfinite (fun i -> Game.EmptyProgram)) 
        |> Seq.take 3
        |> Seq.map (fun p -> Game.ProgramWithAdo 2 p)
        |> Seq.map (fun p -> Game.ProgramWithAdo 3 p)
        |> Seq.map (fun p -> (p.id,p)) 
        |> Map.ofSeq

    static let env = {Game.EmptyEnvironment with programs=programs; processors=processors}

    static let oneTick = TimeSpan.FromSeconds(1.0)

    static member testRuns =

        //test one tick empty
        let outcome = Game.Transform (Tick oneTick) env
        let expected = Success({env with ticks=1})
        let assertion = outcome=expected
        printfn "%b" assertion

        //test one move
        let ado =
            env.programs 
            |> Map.toList
            |> List.head
            |> (fun (a,b) -> b.readyAdos)
            |> Map.toList
            |> List.head
            |> (fun (a,b) -> 
                match b with | Ready x | Running x | Waiting x | Done x -> x)

        let ado2 =
            env.programs 
            |> Map.toList
            |> List.head
            |> (fun (a,b) -> b.readyAdos)
            |> Map.toList
            |> List.skip 1
            |> List.head
            |> (fun (a,b) -> 
                match b with | Ready x | Running x | Waiting x | Done x -> x)

        let ado3 =
            env.programs 
            |> Map.toList
            |> List.skip 2
            |> List.head
            |> (fun (a,b) -> b.readyAdos)
            |> Map.toList
            |> List.head
            |> (fun (a,b) -> 
                match b with | Ready x | Running x | Waiting x | Done x -> x)

        let aProcessor =
            env.processors 
            |> Map.toList
            |> List.head
            |> (fun (a,b) -> b)

        let aProcessor2 =
            env.processors 
            |> Map.toList
            |> List.skip 1
            |> List.head
            |> (fun (a,b) -> b)

        let move a proc lastOutput =
            match lastOutput with
            | Success(e) -> 
                let ot = Game.Transform (Move (a, proc)) e
                printfn "%A" ot
                ot
            | Error(p,e) -> failwith(p)        

        let tick lastOutput =
            let env' = 
                match lastOutput with
                | Success (e) -> e
                | Error(p,e) -> failwith(p)
         
            let output' = Game.Transform (Tick oneTick) env'
            printfn "tick %A" output'
            output'

        let output = Game.Transform (Move (ado3, aProcessor)) env
        printfn "%A" output

        output
        |> move ado aProcessor
        |> move ado2 aProcessor2
        |> tick 
        |> tick 
        |> tick 
        |> ignore
       //TODO: occassionally it works, but mostly the done appears before it should. why?

        Console.ReadKey() |> ignore

