namespace ProcessingGame

open ProcessingGame
open System

type UserRequest =
    | MoveUsing of Ado * Processor
    | TickAmount of int
    | Exit
    | Status
    | Help
    | Error of string

type Message =
    | MoveComplete of ProcessingGame.Environment
    | TickDone of ProcessingGame.Environment
    | ExitReady of string
    | Status of string
    | Help of string
    | Error of string

type MessageRequest = Message * AsyncReplyChannel<ProcessingGame.Environment>

type FullEnvironment = FullEnvironment of UserRequest * Result

type GameInterface() =
    static let (|Int|_|) str =
        match System.Int32.TryParse(str) with
        | (true,int) -> Some(int)
        | _ -> None 

    static let envFromResult = function
        | Success(e) -> e
        | Result.Error(_, e) -> e

    static let tryMatch f v =
        match f with
        | true -> Some(v)
        | false -> None

    static let readyAdoFromId id env =
        let readyMatch status p =
            match status with
            | Ready ado -> tryMatch (ado.id.ToString().StartsWith p) ado
            | _ -> None
        env.programs 
        |> Map.toList
        |> List.tryPick (fun (_, program) -> 
            program.readyAdos
            |> Map.tryPick (fun _ status -> readyMatch status id))    

    static let processorFromId id env =
        env.processors
        |> Map.tryPick (fun k v -> tryMatch (k.ToString().StartsWith id) v)
    
    
    static let tick lastOutput =
        let env' = 
            match lastOutput with
            | Success (e) -> e
            | Result.Error(p,e) -> failwith(p)
         
        let output' = Game.Transform (Tick (TimeSpan.FromSeconds 1.0)) env'
        output'
 
    static let interpretInput (input : string) result =
        let input' = input.Split(' ') |> Array.toList
        let env = envFromResult result
        match input' with
        | [] -> FullEnvironment(UserRequest.Error "Nothing entered", result)
        | [(Int f)] -> 
            if f > 0 then FullEnvironment(TickAmount f, result)
            else FullEnvironment(UserRequest.Error "Ticks msut be greater than 0", result)
        | ["s"] -> FullEnvironment(UserRequest.Status, result)
        | ["e"] -> FullEnvironment(Exit, result)
        | ["i"] -> FullEnvironment(UserRequest.Help, result)
        | p::[r] -> 
            let ado, processor = readyAdoFromId p env, processorFromId r env
            match ado, processor with
            | Some(ado'), Some(processor') ->  
                FullEnvironment(MoveUsing (ado', processor'), result)
            | None, Some(processor') -> 
                FullEnvironment(UserRequest.Error "The process id doesn't exist", result)
            | Some(ado'), None -> 
                FullEnvironment(UserRequest.Error "The processor id doesn't exist", result)
            | None, None ->
                FullEnvironment(UserRequest.Error "The process id and processor id don't exist", result)                
        | _ -> FullEnvironment(UserRequest.Error "Input isn't right. Try again.", result)

//    static let looper (input : MailboxProcessor<MessageRequest>) =
//        let rec looper' (result : Result) = async{
//            let! (request, replyChannel) = input.Receive()
//            match request with
//            | Exit -> replyChannel.Reply Result.Success(
//        } 

    static member intro = 
        let processIntro = "**Each process has a unique id. To select a process, type in the unique id."
        let processIntroHint = "Hint: You only have to type in the first few characters." 
        let processIntroNote = "Note: The program will randomly select a process that starts with that id if there's more than one."
        let processorIntro = "**Each processor has a unique id. Follow the same instructions to select a processor."
        let moveIntro = "**To move a process into a processor, write the process id characters first, then a space, then the processor id characters."
        let moveIntroExample = "For example, if the process id is dfd43ffb-fce9-4e45-8889-88a513aa2264 and the processor id is b02cbf5b-26d8-47fb-a515-4edc4de288d8, then you'd write: dfd4 b02c."
        let moveIntroSuccess = "If the move is successful, the new state will show. Otherwise, you'll receive an error."
        let tickIntro = "**To cause time to proceed, write a number greater than 0 and press Enter. The number is the amount of ticks that will occur."
        let exitIntro = "**To exit, type e."
        let statusIntro = "**To pull up the status of the Programs and their ready proccesses (known as ados) and the Processors, type s."
        let introIntro = "**To bring up this dialog again, press i"
        String.Join("\n", processIntro, processIntroHint, processIntroNote, processorIntro, moveIntro, moveIntroExample, moveIntroSuccess, tickIntro, exitIntro, statusIntro, introIntro);

    static member Play (inputHandler : unit -> string) env = 
        let rec looper result = seq {
            let input = inputHandler()
            let (FullEnvironment (request, result')) = interpretInput input result
            match request with
            | Exit -> yield Message.ExitReady("done")
            | MoveUsing(ado, processor) ->
                let env' = (Game.Transform (Move (ado, processor)) (envFromResult result'))
                match env' with
                | Result.Success e -> yield MoveComplete(e)
                | Result.Error(msg, e) -> yield Message.Error(msg)
                yield! looper env' 
            | (TickAmount a) -> 
                let rec tick' last a' = seq {
                    let latest = tick last
                    if a' - 1 = 0 then yield latest
                    else yield! tick' latest (a'-1)
                }
                let env' = ((tick' result' a) |> Seq.last)
                yield! looper env'
            | (UserRequest.Error error) -> printfn "Error! %A" error; yield! looper result'
            | UserRequest.Status -> printfn "Status:\n %A" result'; yield! looper result'
            | UserRequest.Help -> printfn "%s" GameInterface.intro; yield! looper result'}
        looper env

//    static member PlayAsync (observable : IObservable<'T>) (inputHandler : unit -> string) = 
//        let agent = MailboxProcessor<MessageRequest>.Start(looper)
//        let handler arg =
//            agent.PostAndReply (fun rc -> arg, rc)
//            |> ignore
//        observable.Add(handler)


