namespace ChessPlus

module Finders =
  open Well
  open Maelstrom.WellGuardians
  
  let (<!>) = fun o f -> Option.map f o
  let (<!>>) = fun o f -> o |> Option.map f |> Option.flatten

  let find = id
  
  let findPlayer lifeWell =
    lifeWell.Player
    
  let findDuel lifeWell =
    lifeWell.Duel
    
  let findDuelists lifeWell =
    findDuel lifeWell
    <!> fun d -> d.Duelists
    
  let findDuelist color lifeWell =
    findDuelists lifeWell
    <!>> List.tryFind (fun c -> c = color)
  
  let findBoard lifeWell =
    lifeWell
    |> findDuel
    |> Option.map (fun d -> d.Board)
    
  let findTiles lifeWell =
    lifeWell
    |> findBoard
    <!> fun b -> b.Tiles
    
  let findTile (row, column) lifeWell =
    findBoard lifeWell
    <!> fun b -> b.Tiles
    <!>> Map.tryFind row
    <!>> Map.tryFind column
