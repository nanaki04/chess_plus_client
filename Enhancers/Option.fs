namespace ChessPlus

// MEMO: added for f# 2.0 support
module Option =
  let flatten<'T> v : Option<'T> =
    match v with
    | Some (Some v) -> Some v
    | Some (None) -> None
    | None -> None
    
  let orElse (def : Option<'T>) (v : Option<'T>) : Option<'T> =
    match v with
    | None -> def
    | some -> some
    
  let orElseWith (f : unit -> Option<'T>) (v : Option<'T>) : Option<'T> =
    match v with
    | None -> f ()
    | some -> some
   
  let orElseIf (pred : bool) (def : Option<'T>) (v : Option<'T>) : Option<'T> =
    match v with
    | None -> if pred then def else None
    | some -> some
    
  let orFinally f v =
    match v with
    | None -> f ()
    | _ -> ()  
      
  let apply f v =
    match f, v with
    | Some f, Some v -> Some (f v)
    | _, _ -> None
    
  let defaultValue (def : 'T) (v : Option<'T>) : 'T =
    match v with
    | Some v -> v
    | None -> def
    
  let bind f v : Option<_> =
    match v with
    | Some v -> f v
    | None -> None
    
  let bindAndFlatten<'v> (f : 'v -> Option<_>) (v : Option<'v>) =
    bind f v
    |> flatten
    
  let (>>=) = fun v f -> bind f v
  
  let (>>>=) = fun v f -> bindAndFlatten f v