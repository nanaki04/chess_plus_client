namespace ChessPlus

type Result<'T, 'TError> =
| Ok of 'T
| Error of 'TError

module Result =
  let map f v =
    match v with
    | Error err -> Error err
    | Ok value -> f value |> Ok
    
  let (<!>) = fun f v -> map v f
  
  let apply f v =
    match f, v with
    | Ok f, Ok v -> Ok (f v)
    | Error e, _ -> Error e
    | _, Error e -> Error e
    
  let (<*>) = apply
  
  let merge merger acc v =
    match acc, v with
    | Ok acc, Ok v -> Ok (merger acc v)
    | Error e, _ -> Error e
    | _, Error e -> Error e
    
  let unwrap lst =
    List.fold (merge (fun acc v -> v :: acc)) (Ok []) lst
    <!> List.rev
    
  let orElse v f =
    match v with
    | Error e -> f e
    | v -> v
    
  let orFinally v f =
    match v with
    | Error e -> f e
    | _ -> ()
    
  let (<!!>) = orFinally
  
  let pushOutward v =
    match v with
    | (Some (Ok v)) -> v |> Some |> Ok
    | (Some (Error e)) -> Error e
    | None -> Ok None
    
  let fromOption<'T> o : Result<'T, string> =
    match o with
    | Some v -> Ok v
    | None -> Error "No such value"
    
  let flatten r =
    match r with
    | Ok (Ok v) -> Ok v
    | Ok (Error e) -> Error e
    | Error e -> Error e
    
  let (<!>>) = fun v f -> v <!> f |> flatten