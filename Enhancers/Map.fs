namespace ChessPlus

module Map =
  let first<'k, 'v when 'k : comparison> (f : 'k -> 'v -> bool) (mp : Map<'k, 'v>) : Option<'k * 'v> =
    Map.fold (fun acc k v ->
      match acc with
      | None ->
        if f k v
        then Some (k, v)
        else None
      | r -> r
    ) None mp
    
  let update k updater m =
    match updater (Map.tryFind k m) with
    | Some el -> Map.add k el m
    | None -> Map.remove k m