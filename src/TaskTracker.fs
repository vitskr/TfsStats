namespace fsharpVSO

open Common

type AssigneeInfo = { Planned: double; Worked: double; }

module TaskTracker = 
    open System
    open System.Collections.Generic
    
    type Info() = 
        let mutable currentWork: double = 0.0
        let mutable currentAssignee: string = ""

        let assignees = new Dictionary<string, AssigneeInfo>()

        member this.CurrentWork with get() = currentWork  and set(v) = currentWork <- v    
        member this.CurrentAssignee with get() = currentAssignee and set(v) = currentAssignee <- v

        member this.IsAssigned = String.IsNullOrEmpty(currentAssignee)

        member this.Assignees with get() = assignees
       

    let single (current: Info) (update: Update) = 

        let processAssigneeUpdate (u: AssigneeUpdate) =             
            current.CurrentAssignee <- match u.NewValue with
                                       | Some v -> v
                                       | None -> null

            match u.UpdateType  with 
            | FromNull -> 
                let currentInfo = { Planned = current.CurrentWork; Worked = 0.0 }
                current.Assignees.[u.NewValue.Value] <- currentInfo                
            | ToNull ->
                let currentInfo = current.Assignees.[u.OldValue.Value]
                let adjustedInfo = { currentInfo with Planned = currentInfo.Planned - current.CurrentWork }
                current.Assignees.[u.OldValue.Value] <- adjustedInfo                
            | Change -> 
                let currentInfo = current.Assignees.[u.OldValue.Value]
                let adjustedInfo = { currentInfo with Planned = currentInfo.Planned - current.CurrentWork }
                let newInfo = { Planned = current.CurrentWork; Worked = 0.0 }

                current.Assignees.[u.OldValue.Value] <- adjustedInfo
                current.Assignees.[u.NewValue.Value] <- newInfo

            current

        let processWorkUpdate (u: WorkUpdate) = 
            current.CurrentWork <- match u.NewValue with None -> 0.0 | Some d -> d

            let currentInfo = current.Assignees.GetValue(current.CurrentAssignee)
            match currentInfo with 
            | None -> ()
            | Some info ->
                let adjustedInfo =                    
                    if u.HasRaised then { info with Planned = info.Planned - u.OldValue.Value + u.NewValue.Value }                    
                    else { info with Worked = info.Worked + (u.OldValue.Value - u.NewValue.Value) }                    
                current.Assignees.[current.CurrentAssignee] <- adjustedInfo
            
            current
             
                
        match update with 
        | Assignee assignee -> processAssigneeUpdate assignee
        | Work work -> processWorkUpdate work
        | _ -> printfn "hz what happened"; current
        

    let track (updates : seq<Update>) = 
        let trackInfo = new Info()
        updates
        |> Seq.fold (single) trackInfo