namespace ChessPlus

module Finders =
  open Well
  open Maelstrom.WellGuardians

  let find = id
  
  let findBoard lifeWell =
    lifeWell.Board
    
  let findTile (row, column) lifeWell =
    findBoard lifeWell
    |> fun board -> board.Tiles
    |> Map.tryFind row
    |> function
    | None -> initialTile
    | Some r ->
      r
      |> Map.tryFind column
      |> function
      | None -> initialTile
      | Some t -> t
