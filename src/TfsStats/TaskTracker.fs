module TaskTracker

open Updates

open System
open System.Collections.Generic


type AssigneeInfo = 
    { Planned: double;
      Worked: double; }

type Iteration = string
type Assignee = string

type WorkMap () = 
    let map = new Dictionary<(Iteration * Assignee), AssigneeInfo>()

    member this.SetWork (iteration, assignee, info) =
        map.[(iteration, assignee)] <- info
    
    member this.GetWork (iteration, assignee) = 
        match map.TryGetValue((iteration, assignee)) with
        | true, value -> Some value
        | _ -> None        

    member this.GetStats () : seq<string * string * AssigneeInfo> =
        map
        |> Seq.map (fun (KeyValue(k, v)) -> fst k, snd k, v)

type State() = 

    let mutable currentWork: double = 0.0
    let mutable currentAssignee: string = ""
    let mutable currentIteration: string = ""

    let assignees = WorkMap()
    
    member this.CurrentWork with get() = currentWork  and set(v) = currentWork <- v    
    member this.CurrentAssignee with get() = currentAssignee and set(v) = currentAssignee <- v
    member this.CurrentIteration with get() = currentIteration and set(v) = currentIteration <- v

    member this.Assignees with get() = assignees

    member this.AdjustWork(ammount, ?iteration, ?assignee) = 
        let iteration = defaultArg iteration this.CurrentIteration
        let assignee = defaultArg assignee this.CurrentAssignee

        if not <| (String.IsNullOrEmpty(iteration) || String.IsNullOrEmpty(assignee)) then
            let work = this.GetWork(iteration, assignee)
            let adjusted = match work with 
                            | Some w -> { w with Planned = w.Planned + ammount.Planned; Worked = w.Worked + ammount.Worked }
                            | None -> { Planned = ammount.Planned; Worked = ammount.Worked }
            this.SetWork(adjusted, iteration, assignee)


    member this.GetWork(?iteration, ?assignee) = 
        let iteration = defaultArg iteration this.CurrentIteration
        let assignee = defaultArg assignee this.CurrentAssignee
        
        this.Assignees.GetWork(iteration, assignee)

    member this.GetStats () = 
        assignees.GetStats ()
        
    member private this.SetWork(ammount, ?iteration, ?assignee) =
        let iteration = defaultArg iteration this.CurrentIteration
        let assignee = defaultArg assignee this.CurrentAssignee

        this.Assignees.SetWork(iteration, assignee, ammount)   

let single (state: State) (update: Updates.Update) = 

    let processAssigneeUpdate (u: AssigneeUpdate) = 
        state.CurrentAssignee <- match u.NewValue with
                                    | Some v -> v
                                    | None -> null

        match u.UpdateType with 
        | FromNull ->
            state.AdjustWork (assignee = u.NewValue.Value, ammount = { Planned = state.CurrentWork; Worked = 0.0 })
        | ToNull -> 
            state.AdjustWork (assignee = u.OldValue.Value, ammount = { Planned = -state.CurrentWork; Worked = 0.0 })
        | Changed ->
            state.AdjustWork (assignee = u.OldValue.Value, ammount = { Planned = -state.CurrentWork; Worked = 0.0 })
            state.AdjustWork (assignee = u.NewValue.Value, ammount = { Planned = +state.CurrentWork; Worked = 0.0 })

        state

    let processWorkUpdate (u: WorkUpdate) = 
        state.CurrentWork <- match u.NewValue with None -> 0.0 | Some d -> d
        
        if u.HasRaised then
            let adjustBy = 
                match (u.OldValue, u.NewValue) with
                | (Some old, Some ``new``) -> old + ``new``
                | (Some old, None) -> old
                | (None, Some ``new``) -> ``new``
                | _ -> 0.0
            state.AdjustWork ( { Planned = adjustBy; Worked = 0.0 } )                
        else
            let adjustBy = 
                match (u.OldValue, u.NewValue) with
                | (Some old, Some ``new``) -> old - ``new``
                | (Some old, None) -> old
                | (None, Some ``new``) -> -``new``
                | _ -> 0.0
            state.AdjustWork ({Planned = 0.0; Worked = +adjustBy })
        
        state

    let processIterationUpdate (i: IterationUpdate) = 
        state.CurrentIteration <- match i.NewValue with Some v -> v | None -> null
        
        match i.UpdateType with
        | FromNull ->
            state.AdjustWork (iteration = i.NewValue.Value, ammount = {Planned = state.CurrentWork; Worked = 0.0})
        | ToNull ->
            state.AdjustWork (iteration = i.OldValue.Value, ammount = {Planned = -state.CurrentWork; Worked = 0.0})
        | Changed -> 
            state.AdjustWork (iteration = i.OldValue.Value, ammount = {Planned = -state.CurrentWork; Worked = 0.0})
            state.AdjustWork (iteration = i.NewValue.Value, ammount = {Planned = state.CurrentWork; Worked = 0.0})

        state

    match update with 
    | Assignee assignee -> processAssigneeUpdate assignee
    | Work work -> processWorkUpdate work
    | Iteration iteration -> processIterationUpdate iteration


let track (updates : seq<Update>) = 
    let trackInfo = State()
    updates
    |> Seq.fold (single) trackInfo