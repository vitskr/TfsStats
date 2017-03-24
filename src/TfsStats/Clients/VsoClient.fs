module VsoClient

open System

open Microsoft.VisualStudio.Services.Common
open Microsoft.VisualStudio.Services.Client
open Microsoft.VisualStudio.Services.WebApi

open Microsoft.TeamFoundation.WorkItemTracking.WebApi
open Microsoft.TeamFoundation.WorkItemTracking.WebApi.Models

open Updates

module Fields =
    let [<Literal>] AssignedTo = "System.AssignedTo"
    let [<Literal>] ChangedDate = "System.ChangedDate"
    let [<Literal>] Title = "System.Title"
    let [<Literal>] RemainingWork = "Microsoft.VSTS.Scheduling.RemainingWork"
    let [<Literal>] IterationPath = "System.IterationPath"

let getClient () = 
    let uri = Uri("https://syncplatform.visualstudio.com/DefaultCollection/");
    let connection = new VssConnection(uri, new VssClientCredentials());

    connection.GetClient<WorkItemTrackingHttpClient>()

type Task = { Id : int; ChangedDate : DateTime option }

let getSprintTasks (client: WorkItemTrackingHttpClient) (startDate) (endDate) : seq<int> =

    let itemTypes = seq { yield "Task" }
    let fields = seq { yield Fields.ChangedDate }

    let rec getRevisions (token : string option) = seq {
        let batch = match token with
                    | Some t -> client.ReadReportingRevisionsGetAsync(fields = fields, types=itemTypes, continuationToken = t).Result
                    | None -> client.ReadReportingRevisionsGetAsync(fields = fields, types=itemTypes, startDateTime=Nullable<DateTime>(startDate)).Result

        yield! batch.Values
        if not batch.IsLastBatch then 
            yield! getRevisions(Some batch.ContinuationToken)
    }
    
    let items = 
        getRevisions <| Option<string>.None
        |> Seq.filter (fun revision ->
            if not revision.Id.HasValue then false
            else
                let changed = revision.Fields.GetCastedValue(Fields.ChangedDate)
                if changed.IsNone then false
                else changed.Value < endDate)
        |> Seq.map (fun revision -> revision.Id.Value)
        |> Seq.distinct

    items

let fromField<'a> (field : WorkItemFieldUpdate option) = 
        match field with
        | None -> None
        | Some f -> 
            let oldVal = match f.OldValue with
                            | null -> None
                            | v -> Some (v :?> 'a)
            let newVal = match f.NewValue with
                            | null -> None
                            | v -> Some (v :?> 'a)
            Some (oldVal, newVal)

let getTaskUpdates (client: WorkItemTrackingHttpClient) (id:int) = 
    let updates = 
        client.GetUpdatesAsync(id).Result
        |> Seq.filter (fun (update: WorkItemUpdate) -> not (isNull update.Fields))
        |> Seq.collect (fun (update: WorkItemUpdate) ->
            seq {
                let iteration = fromField<string> <| update.Fields.GetValue(Fields.IterationPath)
                if iteration.IsSome then yield iterationUpdate(iteration.Value)

                let assignee = fromField<string> <| update.Fields.GetValue(Fields.AssignedTo)
                if assignee.IsSome then yield assigneeUpdate(assignee.Value)

                let work = fromField<double> <| update.Fields.GetValue(Fields.RemainingWork)
                if work.IsSome then yield workUpdate(work.Value)
            })
    updates