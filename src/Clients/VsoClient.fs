module TfsStats.VsoClient

module Fields =
    let [<Literal>] AssignedTo = "System.AssignedTo"
    let [<Literal>] ChangedDate = "System.ChangedDate"
    let [<Literal>] Title = "System.Title"
    let [<Literal>] RemainingWork = "Microsoft.VSTS.Scheduling.RemainingWork"
    let [<Literal>] IterationPath = "System.IterationPath"

open Microsoft.TeamFoundation.WorkItemTracking.WebApi
open Microsoft.TeamFoundation.WorkItemTracking.WebApi.Models
open Microsoft.VisualStudio.Services.Common
open Microsoft.VisualStudio.Services.WebApi;
open Microsoft.VisualStudio.Services.Client;
open TfsStats.Updates
open System

let getClient () = 
    let uri = Uri("https://syncplatform.visualstudio.com/DefaultCollection/");
    let connection = new VssConnection(uri, new VssClientCredentials());

    connection.GetClient<WorkItemTrackingHttpClient>()

type Task = { Id : int; ChangedDate : DateTime option }

type System.Collections.Generic.IDictionary<'Key,'Value> with
    member self.GetValue (key:'Key) = // Type extension
        match self.TryGetValue key with
        | true, value -> Some value
        | _ -> None

type System.Collections.Generic.IDictionary<'K, 'V> with
    member self.GetCastedValue (key:'K) : 'U option =
        let (success, v) = self.TryGetValue key
        if (not success)  then None
        else
            let o = v :> obj
            if o :? 'U then Some(o :?> 'U) else None

    member self.ItemEx(key : 'K) : 'U option =
        self.GetCastedValue key
            

let getSprintTasks (client: WorkItemTrackingHttpClient) =

    let itemTypes = seq { yield "Task" }
    let fields = seq { yield Fields.ChangedDate }

    let lastMonday = DateTime.Today.AddDays(-((float DateTime.Today.DayOfWeek) + 5.5))
    let thisMonday = lastMonday.AddDays(7.0)    

    let rec getRevisions (token : string option) = seq {
        let batch = match token with
                    | Some t -> client.ReadReportingRevisionsGetAsync(fields = fields, types=itemTypes, continuationToken = t).Result
                    | None -> client.ReadReportingRevisionsGetAsync(fields = fields, types=itemTypes, startDateTime=Nullable<DateTime>(lastMonday)).Result

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
                else changed.Value < thisMonday)
        |> Seq.map (fun revision -> revision.Id.Value)
        |> Seq.distinct

    items

type Update =
    static member fromField<'a> (field : WorkItemFieldUpdate option) = 
        match field with
        | None -> None
        | Some f -> 
            let oldVal = match f.OldValue with
                         | null -> None
                         | _ as v -> Some (v :?> 'a)
            let newVal = match f.NewValue with
                         | null -> None
                         | _ as v -> Some (v :?> 'a)
            Some (oldVal, newVal)

let getTaskUpdates (client: WorkItemTrackingHttpClient) (id:int) = 
    let updates = 
        client.GetUpdatesAsync(id).Result
        |> Seq.filter (fun (update: WorkItemUpdate) -> not (update.Fields = null))
        |> Seq.collect (fun (update: WorkItemUpdate) ->
            seq {
                let iteration = Update.fromField<string> <| update.Fields.GetValue(Fields.IterationPath)
                if iteration.IsSome then yield iterationUpdate(iteration.Value)

                let assignee = Update.fromField<string> <| update.Fields.GetValue(Fields.AssignedTo)
                if assignee.IsSome then yield assigneeUpdate(assignee.Value)

                let work = Update.fromField<double> <| update.Fields.GetValue(Fields.RemainingWork)
                if work.IsSome then yield workUpdate(work.Value)
            })
    updates