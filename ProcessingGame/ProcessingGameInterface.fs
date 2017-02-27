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
    | Status of string * ProcessingGame.Environment
    | Help of string * ProcessingGame.Environment
    | Error of string * ProcessingGame.Environment

type MessageRequest = UserRequest * AsyncReplyChannel<Message>

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

    static let tick lastOutput =
        let env' = 
            match lastOutput with
            | Success (e) -> e
            | Result.Error(p,e) -> failwith(p)
         
        let output' = Game.Transform (Tick (TimeSpan.FromSeconds 1.0)) env'
        output'
 
    static let UpdatedEnvironment = new Event<_>()
    
    static member ReadyAdoFromId id env =
        let readyMatch status p =
            match status with
            | Ready ado -> tryMatch (ado.id.ToString().StartsWith p) ado
            | _ -> None
        env.programs 
        |> Map.toList
        |> List.tryPick (fun (_, program) -> 
            program.readyAdos
            |> Map.tryPick (fun _ status -> readyMatch status id))    

    static member ProcessorFromId id env =
        env.processors
        |> Map.tryPick (fun k v -> tryMatch (k.ToString().StartsWith id) v)

    static member Play help env (input : MailboxProcessor<MessageRequest>) = 
        let rec looper result = async {
            let! (request, reply) = input.Receive()
            match request with
            | Exit -> reply.Reply(Message.ExitReady("done"))
            | MoveUsing(ado, processor) ->
                let env' = (Game.Transform (Move (ado, processor)) (envFromResult result))
                match env' with
                | Result.Success e -> reply.Reply(Message.MoveComplete(e))
                | Result.Error(msg, e) -> reply.Reply(Message.Error(msg, e))
                do! looper env' 
            | (TickAmount a) -> 
                let rec tick' last a' = seq {
                    let latest = tick last
                    if a' - 1 = 0 then yield latest
                    else yield! tick' latest (a'-1)
                }
                let env' = ((tick' result a) |> Seq.last)
                match env' with
                | Result.Success e -> reply.Reply(Message.TickDone(e))
                | Result.Error(msg, e) -> reply.Reply(Message.Error(msg, e))
                do! looper env'
            | (UserRequest.Error error) -> reply.Reply(Message.Error("Error! " + error, envFromResult result)); do! looper result
            | UserRequest.Status -> reply.Reply(Message.Status(sprintf "Status:\n %A" result, envFromResult result)); do! looper result
            | UserRequest.Help -> reply.Reply(Message.Help(help, envFromResult result)); do! looper result }
        looper env


