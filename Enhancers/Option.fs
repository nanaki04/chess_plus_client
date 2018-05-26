namespace ChessPlus

// MEMO: added for f# 2.0 support
module Option =
  let iter<'T> (f : 'T -> unit) (v : Option<'T>) : Option<'T> =
    Option.map (fun value ->
      f value
      value
    ) v

  let flatten<'T> v : Option<'T> =
    match v with
    | Some (Some v) -> Some v
    | Some (None) -> None
    | None -> None
    
  let unwrap lst =
    List.fold (fun acc v ->
      match acc, v with
      | Some acc, Some v -> v :: acc |> Some
      | Some acc, None -> Some acc
      | _, _ -> None
    ) (Some List.empty) lst
    |> Option.map List.rev
    
  let filter lst =
    List.fold (fun acc v ->
      match v with
      | Some v -> v :: acc
      | _ -> acc
    ) List.empty lst
    |> List.rev
    
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