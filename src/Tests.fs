module Tests

open fsharpVSO

open Xunit
open System
open Swensen.Unquote

type ``Given WorkUpdate that has old work null`` () =
    let Null = Nullable<double>()

    [<Fact>]
    let ``when new work is 12.0 HasRaised should be true`` () =
        let work = WorkUpdate(Null, Nullable(12.0))
        test <@ work.HasRaised = true @>

    [<Fact>]
    let ``when new work is 12.0 HasLowered should be false`` () =
        let work = WorkUpdate(Null, Nullable(12.0))
        test <@ work.HasLowered  = false @>

    [<Fact>]
    let ``when new work is null HasRaised should be false`` () =
        let work = WorkUpdate(Null, Null)        
        test <@ work.HasRaised  = false @>

    [<Fact>]
    let ``when new work is null HasLowered should be false`` () =
        let work = WorkUpdate(Null, Null)
        test <@ work.HasLowered  = false @>

type ``Given WorkUpdate that has new work null`` () =
    let Null = Nullable<double>()

    [<Fact>]
    let ``when old work is 12.0 HasRaised should be false`` () =
        let work = WorkUpdate(Nullable(12.0), Null)
        test <@ work.HasRaised  = false @>

    [<Fact>]
    let ``when old work is 12.0 HasLowered should be true`` () =
        let work = WorkUpdate(Nullable(12.0), Null)
        test <@ work.HasLowered  = true @>

    [<Fact>]
    let ``when old work is null HasRaised should be false`` () =
        let work = WorkUpdate(Null, Null)
        test <@ work.HasRaised  = false @>

    [<Fact>]
    let ``when old work is null HasLowered should be false`` () =
        let work = WorkUpdate(Null, Null)                
        test <@ work.HasLowered  = false @>

type ``Tracker Tests`` () =

    [<Fact>]
    let ``Given assignee changed from null to "worker" info.Assignees should contain "worker"`` () =
        let updates = seq { 
            yield Assignee(AssigneeUpdate(null, "worker")) }
        
        let info = TaskTracker.track updates
        test <@ info.Assignees.ContainsKey("worker") @> 

type ``Given single assignee and work changed from 0 to 6`` () =
    let updates = seq { 
        yield Assignee(AssigneeUpdate(null, "worker"))
        yield Work(WorkUpdate(Nullable<double>(), Nullable<double>(6.0))) }

    [<Fact>]
    let ``Tracker should return planned 6 and worked 0 for that assignee`` () =
        let info = TaskTracker.track updates
        let stats = info.Assignees.["worker"]
        test <@ stats.Planned = 6.0 @>
        test <@ stats.Worked = 0.0 @>

