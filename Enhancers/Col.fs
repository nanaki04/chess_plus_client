namespace ChessPlus

open System.Collections.Generic

type Col<'K, 'V> when 'K : comparison =
| Dict of IDictionary<'K, 'V>
| Map of Map<'K, 'V>

module Col =
  let toMap col =
    match col with
    | Map map -> map
    | Dict dict ->
      dict
      |> Seq.map (|KeyValue|)
      |> Map.ofSeq
      
  let toDict col =
    match col with
    | Dict dict -> dict
    | Map map ->
      map
      |> Map.toSeq
      |> dict
      
  let map f col =
    match col with
    | Map map -> 
      map
      |> Map.toSeq
      |> Seq.map f
      |> Map.ofSeq
      |> Map
    | Dict dct ->
      dct
      |> Seq.map (fun kv -> (kv.Key, kv.Value))
      |> Seq.map f
      |> dict
      |> Dict
      
  let reduce f acc col =
    match col with
    | Map map ->
      map
      |> Map.toSeq
      |> Seq.fold f acc
    | Dict dct ->
      dct
      |> Seq.map (fun kv -> (kv.Key, kv.Value))
      |> Seq.fold f acc
      
  let add k v col =
    match col with
    | Map map -> Map (map.Add (k, v))
    | dct -> 
      dct
      |> toMap
      |> fun map -> Map (map.Add (k, v))
      |> toDict
      |> Dict