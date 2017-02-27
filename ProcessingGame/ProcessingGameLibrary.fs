namespace ProcessingGame
open System
(*
Process is a keyword for future use; so I call Process "Ado".
TEST
Overview of game ...
Programs hold "ados" that take a certain amount of time to process
Processors take "ados" and process them in the order the user places them
Once the user places an ado, it can't be moved
Each tick, the processor processes a certain portion of the ado in it.
If a processor processes all the ado, the ado leaves the processor only if all ados that are a part of the program from which the ado came are also done.
If not, the processor continues to process other ados in it; however, the limit of ados that can fit in it does not decrease.

The number of programs processed in a set number of ticks is the score. The challenge is to separate out the ados in such a way as to get the most programs completed (out of the processors) in the least amount of ticks.

Maybe add "Power" to the Processor to make it more challenging ...
some processors are faster than others
*)



type Ado = {id: Guid; size:int; adone:int; programId: Guid}
type Status =
    | Ready of Ado    //ready to start processing
    | Running of Ado  //not finished processing
    | Waiting of Ado  //finished processing but program still not done
    | Done of Ado     //finished processing and program is done

type Processor = {id: Guid; size:int; ados: Status list; power:int}
type Program = {id: Guid; size:int; readyAdos: Map<Guid,Status>}
type Environment = {programs: Map<Guid,Program>; processors: Map<Guid,Processor>; ticks: int}
type Request = 
    | Move of Ado * Processor
    | Tick of TimeSpan
    | End
type Result =
    | Success of Environment
    | Error of string * Environment

type MaybeBuilder() =
    member x.Bind(m, f) = Option.bind f m
    member x.Return(r) = Some r

type Game() = 

    static let statusSize : (Status -> int) = function 
        Ready y | Running y | Waiting y | Done y -> y.size

    static let statusId : (Status -> Guid) = function
        Ready y | Running y | Waiting y | Done y -> y.id

    static let statusTransform (X : Ado -> Status) = function
        Ready y | Running y | Waiting y | Done y -> X y

    static let statusValue : (Status -> Ado) = function
        Ready y | Running y | Waiting y | Done y -> y

    static let statusValues (s : List<Status>) : List<Ado> =
        s
        |> List.map statusValue
    
    static let statusSizeRunWait : (Status -> int) = function 
        | Ready y | Done y -> 0
        | Running y | Waiting y -> y.size

    static let size (ados : Status list) = 
        ados 
        |> List.sumBy statusSizeRunWait 
    
    static let moveTransform ado (processor : Processor) env = 
        //get the states of "ready" program, ado, and processor
        let maybe = new MaybeBuilder()
        let readyProgramAndAdo = maybe {
            let! program = env.programs.TryFind ado.programId
            let! readyAdo = program.readyAdos.TryFind ado.id
            return (program, readyAdo)
        } 
        let readyProcessor = maybe {
            let! cpu = env.processors.TryFind processor.id
            let! ready = 
                let hasRoom = cpu.size > (size cpu.ados) + ado.size
                if hasRoom then Some(cpu)
                else None
            return ready 
        }

        //direct transformations depending on "ready" states
        match readyProgramAndAdo, readyProcessor with
        | Some (program, s), Some processor' -> 
            let program' = 
                {program with readyAdos = program.readyAdos.Remove (statusId s)}
            let processor'' = 
                let s' = statusTransform Running s
                {processor' with ados = s'::processor'.ados}
            let programs = env.programs |> Map.add program'.id program'
            let processors = env.processors |> Map.add processor''.id processor''
            Success({env with programs=programs; processors=processors})
        | _, None -> 
            Error("Processor can't accept process!", env)
        | None, _ ->
            Error("Program doesn't have requested process!", env)

    static let tickAdo power adoStatus =
        match adoStatus with
        | (Running ado') -> 
            let minAdd = Math.Min((ado'.size - ado'.adone), power)
            let leftOver = power - minAdd
            let ado'' = {ado' with adone=ado'.adone + minAdd}
            if ado''.adone >= ado''.size then (leftOver, Waiting ado'')
            else (leftOver, Running ado'')
        | _ -> (power, adoStatus)

    //transforms ado by ticking if the previous isn't currently Running
    static let tickAdoProcess power ados =
        let rec looper powerLeft ados' result = seq{
            match powerLeft, ados' with
            | _, [] -> yield result
            | 0, _ -> yield ados'@(result |> List.rev)
            | _, f::rest -> 
                let powerLeft', adoStatus' = tickAdo powerLeft f
                yield! looper powerLeft' rest (adoStatus'::result)
        }
        (looper power ados []) |> Seq.last

    static let tickProcessor k p : Processor =
        {p with ados = p.ados |> List.rev |> tickAdoProcess p.power} 

    static let theRunners (k, p) =
        p.ados
        |> List.choose (fun x->match x with | Running r -> Some(r) | _ -> None)
        |> Set.ofList

    //the ados that are waiting 
    static let theWaiters (k, p) =
        p.ados
        |> List.choose (fun x->match x with | Waiting w -> Some(w) | _ -> None)
        |> Set.ofList

    //the ados that are ready in a program but aren't being processed yet
    static let theReadies (k, p) =
        p.readyAdos 
        |> Map.toSeq
        |> Seq.map (fun (k', s) -> statusValue s)

    //order matters for processing, which is why dones are grouped together
    static let transformDones (dones : Guid list) p =
        let switch ds = function
            | Ready x -> Ready x
            | Running x -> Running x
            | Waiting x when ds |> List.exists (fun d -> d = x.programId) -> Done x
            | Waiting x -> Waiting x
            | Done x -> Done x     
        let switched = p.ados |> List.map (switch dones)

        let (|NotComplete|Complete|) = function
            | Ready _ | Running _ | Waiting _ -> NotComplete
            | Done _ -> Complete
        let categorize = (|NotComplete|Complete|)
        let categorized = switched |> List.groupBy categorize |> Map.ofList
        
        let notDones = categorized.TryFind (Choice<unit,unit>.Choice1Of2())
        let dones' = categorized.TryFind (Choice<unit,unit>.Choice2Of2())

        match dones', notDones with
        | Some(d), Some(n) -> {p with ados=n@d}
        | Some(d), None -> {p with ados=d}
        | _, _ -> p
    
    static let filteredAdos condition lat =
        lat
        |> Map.toSeq
        |> Seq.collect condition
        |> Set.ofSeq
        |> Set.map (fun r -> r.programId)

    static let tickTransform env =
        //this is where you tick by one ... if the process is done, then it leaves
        //the processor if all program's ados are waiting
        let processors = env.processors |> Map.map tickProcessor
        let newlyDones = 
            (filteredAdos theWaiters processors) - (filteredAdos theRunners processors) - (filteredAdos theReadies env.programs)
            |> Set.toList
        match newlyDones.IsEmpty with
        | true -> Success({env with processors=processors; ticks=env.ticks+1})
        | false ->   
            processors
            |> Map.map (fun _ p -> if p.ados.IsEmpty then p else transformDones newlyDones p)
            |> (fun processors' -> Success({env with processors=processors'; ticks=env.ticks+1}))

    //region -- constructors, etc.

    static member EmptyProgram =
        {id=Guid.NewGuid(); size=0; readyAdos=Map.empty}

    static member ProgramWithAdo adoSize program =
        let ado = {id=Guid.NewGuid(); size=adoSize; adone=0; programId=program.id}
        {program with readyAdos=program.readyAdos.Add(ado.id, Ready(ado));  size=program.size+ado.size}
     
    static member EmptyProcessor size power =
        {id=Guid.NewGuid(); size=size; ados=[]; power=power}

    static member EmptyEnvironment =
        {programs=Map.empty; processors=Map.empty; ticks=0}

    static member BuildMoveRequest ado processor =
        Move(ado,processor)

    static member BuildTickRequest amount =
        Tick(amount)

    static member BuildEndRequest =
        End

    static member Transform request env =
        match request with
        | Move(ado, processor) -> moveTransform ado processor env
        | Tick(n) -> tickTransform env
        | End -> Success(env)
