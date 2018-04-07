﻿namespace ChessPlus

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
    
  let apply f v =
    match f, v with
    | Some f, Some v -> Some (f v)
    | _, _ -> None
    
  let defaultValue (def : 'T) (v : Option<'T>) : 'T =
    match v with
    | Some v -> v
    | None -> def