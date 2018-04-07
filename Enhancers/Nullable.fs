namespace ChessPlus

type Nullable<'T> = 'T

module Nullable =
  let toResult<'T when 'T : equality> n =
    if Unchecked.defaultof<'T> = n
    then Error "Null value input"
    else Ok n
    
  let fromResult<'T> r =
    match r with
    | Error _ -> Unchecked.defaultof<'T>
    | Ok n -> n

  let toOption<'T when 'T : equality> n =
    if Unchecked.defaultof<'T> = n
    then None
    else Some n
    
  let fromOption<'T> o =
    match o with
    | None -> Unchecked.defaultof<'T>
    | Some o -> o
    
  let maybe f v =
    match v with
    | null -> ()
    | v -> f v
    
  let map (f : 'a -> 'b) (v : 'a) =
    if Unchecked.defaultof<'a> = v
    then Unchecked.defaultof<'b>
    else f v