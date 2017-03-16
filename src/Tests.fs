module Tests

open fsharpVSO

open Xunit
open Swensen.Unquote
open System

let workUpdate (oldValue, newValue) = 
    Work(WorkUpdate(oldValue, newValue))

let assigneeUpdate (oldValue, newValue) = 
    Assignee(AssigneeUpdate(oldValue, newValue))

let iterationUpdate (oldValue, newValue) =
    Iteration(IterationUpdate(oldValue, newValue))

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

type ``Given assignee 'worker1' and work changed from 0 to 6`` () =
    let updates = seq { 
        yield iterationUpdate(null, "iteration1")
        yield assigneeUpdate(null, "worker1")
        yield workUpdate(Nullable<double>(), Nullable<double>(6.0)) }

    [<Fact>]
    let ``Tracker should return worker1: { p:6, w:0 }`` () =
        let info = TaskTracker.track updates
        let stats = info.GetWork(assignee = "worker1").Value
        test <@ stats.Planned = 6.0 @>
        test <@ stats.Worked = 0.0 @>

    [<Fact>]
    let ``CurrentWork should be 6`` () = 
        let info = TaskTracker.track updates
        test<@ info.CurrentWork = 6.0 @>

    let ``And work changes from 6 to 8 current work should be 8.0`` () =
        let updates = Seq.append updates <| seq { yield workUpdate(Nullable<double>(6.0), Nullable<double>(8.0)) }
        let info = TaskTracker.track updates
        test <@ info.CurrentWork = 8.0 @>

    [<Fact>]
    let ``and then work changes from 6 to 4 tracker should return worker1: {p: 6, w: 2}`` () =
        let updates = Seq.append updates <| seq { yield workUpdate(Nullable<double>(6.0), Nullable<double>(4.0)) }
        let info = TaskTracker.track updates
        let stats = info.GetWork(assignee = "worker1").Value
        test <@ stats.Planned = 6.0 @>
        test <@ stats.Worked = 2.0 @>

    [<Fact>]
    let ``and then assignee changed to 'worker2' Tracker should return worker1: {p:0, w:0}, worke2: { p:6, w:0 }`` () =
        let newUpdates = seq { yield assigneeUpdate("worker1", "worker2") } |> Seq.append updates
        let info = TaskTracker.track newUpdates
        let statsWorker1 = info.GetWork(assignee = "worker1").Value
        test <@ (statsWorker1.Planned, statsWorker1.Worked) = (0.0, 0.0) @>

        let statsWorker2 = info.GetWork(assignee = "worker2").Value
        test <@ (statsWorker2.Planned, statsWorker2.Worked) = (6.0, 0.0) @>

    [<Fact>]
    let ``and then work changes from 6 to 4 and then assignee changes to worker2. worker1: {p:2; w:2}. worker2: {p:4;w:0}`` () =
        let updates = 
            Seq.append updates 
                <| seq {  yield workUpdate(Nullable<double>(6.0), Nullable<double>(4.0))
                          yield assigneeUpdate("worker1", "worker2") }

        let info = TaskTracker.track updates

        let actual1 = info.GetWork(assignee="worker1")
        let actual2 = info.GetWork(assignee="worker2")

        let expected1 = Some { Planned = 2.0; Worked = 2.0}
        let expected2 = Some {Planned = 4.0; Worked = 0.0}

        test <@ actual1 = expected1 @>
        test <@ actual2 = expected2 @>

    [<Fact>]
    let ``and then w 6 => 4 and then a worker1 => worker2  and then w 4 => 0. w2: { p:4.0 w:4.0 }`` () = 
        let updates = 
            Seq.append updates 
                <| seq {  yield workUpdate(Nullable<double>(6.0), Nullable<double>(4.0))
                          yield assigneeUpdate("worker1", "worker2") 
                          yield workUpdate(Nullable<double>(4.0), Nullable<double>(0.0))}

        let info = TaskTracker.track updates

        let actual1 = info.GetWork(assignee="worker1")
        let actual2 = info.GetWork(assignee="worker2")

        let expected1 = Some { Planned = 2.0; Worked = 2.0}
        let expected2 = Some {Planned = 4.0; Worked = 4.0}

        test <@ actual1 = expected1 @>
        test <@ actual2 = expected2 @>

type ``Given no iteration, w 0 => 6, assignee changes null => 'w1'  '`` () = 
    let updates = 
        seq {
            yield workUpdate(Nullable<double>(), Nullable<double>(6.0))
            yield assigneeUpdate(null, "w1") }        

    [<Fact>]
    let ``w1: planned should be 0`` () = 
        let info = TaskTracker.track updates
        let actual = info.GetWork(assignee="w1")
        test <@ actual.IsNone @>