namespace ProcessingGame
open System
(*
Process is a keyword for future use; so I call Process "Ado".

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

type Status<'a> =
    | Ready of 'a    //ready to start processing
    | Running of 'a  //not finished processing
    | Waiting of 'a  //finished processing but program still not done
    | Done of 'a     //finished processing and program is done

type Ado = {id: Guid; size:int; adone:int; programId: Guid}
type Processor = {id: Guid; size:int; ados: Status<Ado> list; power:int}
type Program = {id: Guid; size:int; readyAdos: Map<Guid,Status<Ado>>}
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

    static let statusSize : (Status<Ado> -> int) = function 
        Ready y | Running y | Waiting y | Done y -> y.size

    static let statusId : (Status<Ado> -> Guid) = function
        Ready y | Running y | Waiting y | Done y -> y.id

    static let statusTransform (X : Ado -> Status<Ado>) = function
        Ready y | Running y | Waiting y | Done y -> X y

    static let statusValue : (Status<Ado> -> Ado) = function
        Ready y | Running y | Waiting y | Done y -> y

    static let statusValues (s : List<Status<Ado>>) : List<Ado> =
        s
        |> List.map statusValue
    
    static let statusSizeRunWait : (Status<Ado> -> int) = function 
        | Ready y | Done y -> 0
        | Running y | Waiting y -> y.size

    static let size (ados : Status<Ado> list) = 
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
        | Running ado' -> 
            let ado'' = {ado' with adone=ado'.adone + power}
            if ado''.adone >= ado''.size then Waiting ado''
            else Running ado''
        | _ -> adoStatus

    //transforms ado by ticking if the previous isn't currently Running
    static let tickAdoFolder power state adoStatus =
        match state with
        | [] -> [tickAdo power adoStatus]
        | f::rest -> 
            match f with
            | Running _ -> adoStatus::state //previous already running, so skip
            | _ -> (tickAdo power adoStatus)::state

    static let tickProcessor k p : Processor =
        let tickAdoFolder' = tickAdoFolder p.power
        {p with ados = p.ados |> List.fold tickAdoFolder' []}

    static let theRunners (k, p) =
        p.ados
        |> List.choose (fun x->match x with | Running r -> Some(r) | _ -> None)
        |> Set.ofList

    //the ados that are waiting 
    static let theWaiters (k, p) =
        p.ados
        //note ... there can be more than one waiting in a process
        |> List.choose (fun x->match x with | Waiting w -> Some(w) | _ -> None)
        |> Set.ofList

    //the ados that are ready in a program but aren't being processed yet
    static let theReadies (k, p) =
        p.readyAdos 
        |> Map.toSeq
        |> Seq.map (fun (k', s) -> statusValue s)

    static let correctlyPlacedDone d adosRev =
        let rec f front back =
            match back with
            | [] -> (front |> List.rev)@[d]  //this is the first one
            | (Done x)::rest -> (front |> List.rev)@[d]@back //place 
            | x::rest -> f (x::front) rest //still looking for a done or first []
        f [] adosRev

    static let donesFirstComparer x y =
        match x, y with
        | Done _, Done _ -> 0
        | _, Done _ -> 1
        | Done _, _ -> -1
        | _, _ -> 0  

    static let donesFirst (k : Guid) p = 
        let ados = 
            p.ados 
            |> List.sortWith donesFirstComparer
        {p with ados=ados}

    //order matters for processing, which is why dones are grouped together
    static let transformDones dones k p =
        let rec transformer result v =
            match v with
            | [] -> result |> List.rev
            | (Waiting f)::rest when dones |> Set.contains f.programId ->
                transformer ((Done f)::result) rest 
            | f::rest -> transformer (f::result) rest 
        {p with ados=transformer [] p.ados}

    static let tickTransform env =
        //this is where you tick by one ... if the process is done, then it leaves
        //the processor if all program's ados are waiting
        let processors = env.processors |> Map.map tickProcessor
        let runnerAdos = 
            processors
            |> Map.toSeq
            |> Seq.collect theRunners
            |> Set.ofSeq
            |> Set.map (fun r -> r.programId)
        let waitAdos = 
            processors 
            |> Map.toSeq 
            |> Seq.collect theWaiters 
            |> Set.ofSeq
            |> Set.map (fun w -> w.programId)
        let readyAdos = //what's still ready to start processing
            env.programs 
            |> Map.toSeq 
            |> Seq.collect theReadies
            |> Set.ofSeq
            |> Set.map (fun r -> r.programId)
        let newlyDoneIds = waitAdos - runnerAdos - readyAdos
        let processors' = 
            processors 
            |> Map.map (transformDones newlyDoneIds)
            |> Map.map donesFirst 
        Success({env with processors=processors'; ticks=env.ticks+1})

    //region -- constructors, etc.

    static member emptyProgram =
        {id=Guid.NewGuid(); size=0; readyAdos=Map.empty}

    static member programWithAdo adoSize program =
        let ado = {id=Guid.NewGuid(); size=adoSize; adone=0; programId=program.id}
        {program with readyAdos=program.readyAdos.Add(ado.id, Ready(ado));  size=program.size+ado.size}
     
    static member emptyProcessor size power =
        {id=Guid.NewGuid(); size=size; ados=[]; power=power}

    static member emptyEnvironment =
        {programs=Map.empty; processors=Map.empty; ticks=0}

    static member buildMoveRequest ado processor =
        Move(ado,processor)

    static member buildTickRequest amount =
        Tick(amount)

    static member buildEndRequest =
        End

    static member transform request env =
        match request with
        | Move(ado, processor) -> moveTransform ado processor env
        | Tick(n) -> tickTransform env
        | End -> Success(env)
