module Program 

open Argu

open Updates
open TaskTracker
open VsoClient

open NLog
open NLog.FSharp

open System

type Args = 
    | [<Mandatory>] StartDate of string
    | [<Mandatory>] EndDate of string
    | Sprint of string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | StartDate _ -> "specify Start Date in yyyy-MM-ddThh:mm:ss format"
            | EndDate _ -> "specify Start Date in yyyy-MM-ddThh:mm:ss format"
            | Sprint _ -> "specify IterationPath"
    

[<EntryPoint>]
let main argv = 
    let logger = Logger(NLog.LogManager.GetCurrentClassLogger())

    let parser = ArgumentParser.Create<Args>(programName = "tfsstats.exe", errorHandler = ProcessExiter())
    let results = parser.ParseCommandLine(argv, raiseOnUsage = true)

    let parseDate s = 
        DateTime.ParseExact(s, "yyyy-MM-ddTHH:mm:ss", Globalization.CultureInfo.InvariantCulture)

    let startDate = results.PostProcessResult (<@ StartDate @>, parseDate)
    let endDate = results.PostProcessResult (<@ EndDate @>, parseDate)

    let iteration = results.TryPostProcessResult (<@ Sprint @>, fun (s) -> s.ToLower())

    use client = getClient ()

    let tasks = getSprintTasks client startDate endDate
    let stats = seq {
        for task in tasks do
            logger.Trace "Fetching changes for task: %i" task

            let updates = getTaskUpdates client task
            let info = TaskTracker.track updates

            let stats = info.GetStats()
            yield! stats
    }
        
    let groupedBy = Seq.groupBy (fun (f, s, t) -> f, s) stats
    let summed =
        groupedBy
        |> Seq.map (fun (f,s) -> 
            f, Seq.fold (fun acc (_,_,w) -> 
                            { Planned = acc.Planned + w.Planned; Worked = acc.Worked + w.Worked }) { Planned = 0.0; Worked = 0.0 } s)

    let filter x = 
        match iteration with
        | Some s ->  String.Compare(fst (fst x), s, true) = 0; 
        | None -> true

    summed
    |> Seq.filter filter
    |> Seq.iter (fun s -> printfn "%A" s)

    0 // return an integer exit code
