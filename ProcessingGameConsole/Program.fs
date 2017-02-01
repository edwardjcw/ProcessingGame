// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open ProcessingGame
open System

let processors = 
    (Seq.initInfinite (fun i -> Game.emptyProcessor 10 1)) 
    |> Seq.take 5
    |> Seq.map (fun p -> (p.id, p)) 
    |> Map.ofSeq

let programs = 
    (Seq.initInfinite (fun i -> Game.emptyProgram)) 
    |> Seq.take 3
    |> Seq.map (fun p -> Game.programWithAdo 2 p)
    |> Seq.map (fun p -> Game.programWithAdo 3 p)
    |> Seq.map (fun p -> (p.id,p)) 
    |> Map.ofSeq

let env = {Game.emptyEnvironment with programs=programs; processors=processors}

let oneTick = TimeSpan.FromSeconds(1.0)


[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    //test one tick empty
    let outcome = Game.transform (Tick oneTick) env
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
            let ot = Game.transform (Move (a, proc)) e
            printfn "%A" ot
            ot
        | Error(p,e) -> failwith(p)        

    let tick lastOutput =
        let env' = 
            match lastOutput with
            | Success (e) -> e
            | Error(p,e) -> failwith(p)
         
        let output' = Game.transform (Tick oneTick) env'
        printfn "tick %A" output'
        output'

    let output = Game.transform (Move (ado3, aProcessor)) env
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
    0 // return an integer exit code
