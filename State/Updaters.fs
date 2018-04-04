namespace ChessPlus

module Updaters =
  open Well
  open UnityEngine
  
  let update updater well =
    updater well
    
  let updateBoard (updater : Board -> Board) (well : LifeWell) =
    { well with Board = updater well.Board }
    
  let updateTiles updater well =
    (fun board ->
      { board with Tiles = updater board.Tiles }
    )
    |> updateBoard <| well

  let updateTile (row, column) updater well =
    (fun tiles ->
      let r = Map.tryFind row tiles |> function
        | Some r -> r
        | None -> Map.empty
      let tile = Map.tryFind column r |> function
        | Some t -> t
        | None -> initialTile
      Map.add column (updater tile) r
      |> Map.add row <| tiles
    )
    |> updateTiles <| well
