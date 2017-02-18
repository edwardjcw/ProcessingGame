namespace ProcessingGame

type EnvironmentBuilder() =
    static let processors = 
        (Seq.initInfinite (fun i -> Game.EmptyProcessor 10 1)) 
        |> Seq.take 5
        |> Seq.map (fun p -> (p.id, p)) 
        |> Map.ofSeq

    static let programs = 
        (Seq.initInfinite (fun i -> Game.EmptyProgram)) 
        |> Seq.take 3
        |> Seq.map (
            (fun p ->Game.ProgramWithAdo 2 p)
            >> (fun p ->Game.ProgramWithAdo 3 p)
            >> (fun p ->p.id,p))
        |> Map.ofSeq

    static member SampleEnv = {Game.EmptyEnvironment with programs=programs; processors=processors}

