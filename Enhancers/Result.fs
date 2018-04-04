namespace ChessPlus

type Result<'T, 'TError> =
| Ok of 'T
| Error of 'TError

module Result =
  let map v f =
    match v with
    | Error err -> Error err
    | Ok value -> f value |> Ok
    
  let (<!>) = map
  
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
    
  let orElse v f =
    match v with
    | Error e -> f e
    | v -> v
    
  let orFinally v f =
    match v with
    | Error e -> f e
    | _ -> ()
    
  let (<!!>) = orFinally