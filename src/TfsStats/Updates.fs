module Updates

open System

type UpdateTypes = 
    | FromNull
    | ToNull
    | Changed

[<AbstractClass>]
type UpdateBase<'a>(oldValue: 'a option, newValue: 'a option) = 
    member this.OldValue = oldValue
    member this.NewValue = newValue

    member this.UpdateType = 
        match (this.OldValue, this.NewValue) with 
        | (Some _, None _) -> ToNull
        | (None _, Some _) -> FromNull
        | _ -> Changed

    override this.ToString() = 
        sprintf "Change from %A to %A" this.OldValue this.NewValue


type WorkUpdate(oldValue: double option, newValue: double option) =
    inherit UpdateBase<double>(oldValue, newValue)

    member this.HasRaised = this.NewValue > this.OldValue
    member this.HasLowered = this.OldValue > this.NewValue

type AssigneeUpdate(oldValue: string option, newValue: string option)
    = inherit UpdateBase<string>(oldValue, newValue)

type IterationUpdate(oldValue: string option, newValue: string option)
    = inherit UpdateBase<string>(oldValue, newValue)

type Update = 
    | Work of WorkUpdate
    | Assignee of AssigneeUpdate
    | Iteration of IterationUpdate

let workUpdate (oldValue, newValue) : Update = 
    Work(WorkUpdate(oldValue, newValue))

let assigneeUpdate (oldValue, newValue) : Update = 
    Assignee(AssigneeUpdate(oldValue, newValue))

let iterationUpdate (oldValue, newValue) : Update =
    Iteration(IterationUpdate(oldValue, newValue))