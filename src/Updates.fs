namespace fsharpVSO

open Common
open System

type UpdateType = 
    | FromNull
    | ToNull
    | Change

[<AbstractClass>]
type UpdateBase<'a>(oldValue: 'a option, newValue: 'a option) = 
    member this.OldValue = oldValue
    member this.NewValue = newValue

    member this.UpdateType = 
        match this.OldValue with 
        | Some _ when this.NewValue.IsNone -> ToNull
        | None when this.NewValue.IsSome -> FromNull
        | _ -> Change

    override this.ToString() = 
        sprintf "Change from %A to %A" this.OldValue this.NewValue


type WorkUpdate(oldValue: Nullable<double>, newValue: Nullable<double>) =
    inherit UpdateBase<double>(Option.fromNullable(oldValue), Option.fromNullable(newValue))

    member this.HasRaised = this.NewValue > this.OldValue
    member this.HasLowered = this.OldValue > this.NewValue


type AssigneeUpdate(oldValue: string, newValue: string)
    = inherit UpdateBase<string>(Option.fromString(oldValue), Option.fromString(newValue))

type IterationUpdate(oldValue: string, newValue: string)
    = inherit UpdateBase<string>(Option.fromString(oldValue), Option.fromString(newValue))


type Update = 
    | Work of WorkUpdate
    | Assignee of AssigneeUpdate
    | Iteration of IterationUpdate