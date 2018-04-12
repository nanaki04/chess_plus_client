namespace ChessPlus

type Matrix<'r, 'c, 'v when 'r : comparison and 'c : comparison> = Map<'r, Map<'c, 'v>>

module Matrix =
  let retn r c v =
    Map.empty.Add(
      r,
      Map.empty.Add(c, v)
    )
    
  let add r c v m =
    m
    |> Map.tryFind r
    |> function
      | None -> retn r c v
      | Some row -> m.Add(r, row.Add(c, v))

  let private mapColumns f cols =
    Map.map (fun col v -> f v) cols
    
  let private mapRows f rows =
    Map.map (fun _ cols -> mapColumns f cols) rows

  let map f m =
    mapRows f m
    
  let private foldColumns<'c, 'v, 's when 'c : comparison> f (s : 's) r (cols : Map<'c, 'v>) =
    Map.fold (fun state c v -> f s r c v) s cols
    
  let private foldRows f s rows =
    Map.fold (fun state row cols -> foldColumns f state row cols) rows
    
  let fold f s m =
    foldRows f s m
    
  let tryFind r c m =
    m
    |> Map.tryFind r
    |> Option.map (Map.tryFind c)
    
  let private filterColumns p r (cols : Map<_, _>) =
    foldColumns (fun (s : Map<_, _>) _ c v ->
      if p v then s.Add(c, v) else s
    ) Map.empty r cols
    
  let filter p (m : Matrix<_, _, _>) =
    Map.fold (fun (s : Matrix<_, _, _>) r (cols : Map<_, _>) ->
      let filteredCols = filterColumns p r cols
      if Map.isEmpty filteredCols then s else s.Add(r, filteredCols)
    ) Map.empty m
    
  let first f m =
    fold (fun s _ _ v ->
      match s with
      | Some v -> Some v
      | None -> f v
    ) None m
    
  let update r c f m =
    m
    |> tryFind r c
    |> (fun res -> add r c (f res) m)
    
  let updateWhere p f m =
    map (fun v ->
      if p v then f v else v
    ) m