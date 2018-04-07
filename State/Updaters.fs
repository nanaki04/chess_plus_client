namespace ChessPlus

module Updaters =
  open Well
  open UnityEngine
  
  let update updater well =
    updater well
    
  let updatePlayer (updater : Player -> Player) (well : LifeWell) =
    { well with Player = updater well.Player }
    
  let updateDuel (updater : Option<Duel> -> Option<Duel>) (well : LifeWell) =
    { well with Duel = updater well.Duel }
  
  let updateDuelists (updater : Duelist list -> Duelist list) (well : LifeWell) =
    well
    |> updateDuel (Option.map (fun d -> { d with Duelists = updater d.Duelists }))
  
  let updateDuelist color (updater : Duelist -> Duelist) (well : LifeWell) =
    well
    |> updateDuelists (List.map (fun d -> if d.Color = color then updater d else d))

  let updateBoard (updater : Board -> Board) (well : LifeWell) =
    well
    |> updateDuel (Option.map (fun d -> { d with Board = updater d.Board }))
    
  let updateTiles updater well =
    well
    |> updateBoard (fun b -> { b with Tiles = updater b.Tiles })

  let updateTile (row, column) updater well =
    (fun tiles ->
      let r = Map.tryFind row tiles |> function
        | Some r -> r
        | None -> Map.empty
      let tile = Map.tryFind column r |> function
        | Some t -> t
        | None -> Tile.initial
      Map.add column (updater tile) r
      |> Map.add row <| tiles
    )
    |> updateTiles <| well
    
  let updatePiece coord updater well =
    well
    |> updateTile coord (fun t -> { t with Piece = updater t.Piece })
