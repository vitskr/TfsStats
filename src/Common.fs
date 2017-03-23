[<AutoOpen>]
module Option

open System

let fromNullable (n: _ Nullable) = 
    if n.HasValue
        then Some n.Value
        else None

let fromNull n = 
    match box n with 
    | null -> None 
    | a -> Some a

let fromString (n: String) = 
    if String.IsNullOrEmpty <| n then 
        None
    else 
        Some n

let convertsTo<'a> candidate = 
    match box candidate with 
    | :? 'a as converted -> Some converted
    | _ -> None

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