namespace ChessPlus

type Matrix<'r, 'c, 'v when 'r : comparison and 'c : comparison> = Map<'r, Map<'c, 'v>>

module Matrix =
  open System.Collections.Generic
  
  let empty = Map.empty

  let retn r c v =
    Map.empty.Add(
      r,
      Map.empty.Add(c, v)
    )
    
  let add r c v m =
    m
    |> Map.tryFind r
    |> function
      | None ->
        m.Add(r, Map.empty.Add(c, v))
      | Some row ->
        m.Add(r, row.Add(c, v))

  let private mapColumns f cols =
    Map.map (fun col v -> f v) cols
    
  let private mapRows f rows =
    Map.map (fun _ cols -> mapColumns f cols) rows

  let map f m =
    mapRows f m
    
  let mapRC f m =
    Map.map (fun r cols ->
      Map.map (fun c item -> f r c item) cols
    ) m
    
  let private foldColumns f s r cols =
    Map.fold (fun state c v -> f state r c v) s cols
    
  let fold f s m =
    Map.fold (fun state row cols -> foldColumns f state row cols) s m
    
  let tryFind r c m =
    m
    |> Map.tryFind r
    |> Option.bind (Map.tryFind c)
    
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
    
  let firstRC f m =
    fold (fun s r c v ->
      match s with
      | Some v -> Some v
      | None -> f r c v
    ) None m
      
  let update r c f m =
    m
    |> tryFind r c
    |> fun res -> f res
    |> Option.map (fun v -> add r c v m)
    |> Option.defaultValue m
    
  let updateWhere p f m =
    map (fun v ->
      if p v then f v else v
    ) m
    
  let fromDict<'r, 'c, 't when 'r : comparison and 'c : comparison> (d : IDictionary<'r, IDictionary<'c, 't>>) =
    d
    |> Seq.map (|KeyValue|)
    |> Map.ofSeq
    |> Map.map (fun _ (v : IDictionary<'c, 't>) ->
       Seq.map (|KeyValue|) v
       |> Map.ofSeq
    )
      