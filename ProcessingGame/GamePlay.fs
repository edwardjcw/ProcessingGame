namespace ProcessingGame

open ProcessingGame
open System

type GamePlay() =

    static let intro = 
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

    static member InterpretInput (input : string) checkForAdo checkForProcessor =
        let input' = input.Split(' ') |> Array.toList
        match input' with
        | [] -> UserRequest.Error "Nothing entered"
        | [(Int f)] -> 
            if f > 0 then UserRequest.TickAmount f
            else UserRequest.Error "Ticks msut be greater than 0"
        | ["s"] -> UserRequest.Status
        | ["e"] -> Exit
        | ["i"] -> UserRequest.Help
        | p::[r] -> 
            let ado, processor = checkForAdo p, checkForProcessor r
            match ado, processor with
            | Some(ado'), Some(processor') ->  
                MoveUsing (ado', processor')
            | None, Some(processor') -> 
                UserRequest.Error "The process id doesn't exist"
            | Some(ado'), None -> 
                UserRequest.Error "The processor id doesn't exist"
            | None, None ->
                UserRequest.Error "The process id and processor id don't exist"               
        | _ -> UserRequest.Error "Input isn't right. Try again."

    static member Start = 
        printfn "%s" intro

        let h = intro
        let env = EnvironmentBuilder.SampleEnv
        let startResult = Success env
        let game mp = GameInterface.Play h startResult mp
        let agent = MailboxProcessor<MessageRequest>.Start(game)
        let rec looper' r = seq { 
            let input = Console.ReadLine()
            let adoCheck x = GameInterface.ReadyAdoFromId x r
            let processorCheck x = GameInterface.ProcessorFromId x r
            let interpretedInput = GamePlay.InterpretInput input adoCheck processorCheck
            let result = agent.PostAndReply(fun rc -> interpretedInput, rc)
            match result with
            | Message.Error(m,e) | Message.Status(m,e) | Message.Help(m,e) -> printfn "%A" m; yield! looper' e
            | Message.TickDone e | MoveComplete e -> printfn "Operation complete"; yield! looper' e
            | Message.ExitReady m -> printfn "%A" m; yield ()
        }
        looper' env