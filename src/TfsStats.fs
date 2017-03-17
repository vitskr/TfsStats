module TfsStats.Program

open TfsStats.VsoClient

[<EntryPoint>]
let main argv =          
    use client = getClient ()

    let tasks = getSprintTasks client
    for task in tasks do
        printfn "Fetching changes for task: %i" <| task

        let updates = getTaskUpdates client task
        let info = TaskTracker.track updates

        info.GetStats() 
        |> Seq.iter (fun stats -> printfn "%A" stats)
    
    printfn "Hello"

    0 // return an integer exit code
