namespace ChessPlus

module Finders =
  open Well
  open Maelstrom.WellGuardians
  open Option
  
  let (<!>) = fun o f -> Option.map f o

  let find = id
  
  let findPlayer lifeWell =
    lifeWell.Player
    
  let findDuel lifeWell =
    lifeWell.Duel
    
  let findConnection lifeWell =
    lifeWell.Connection
    
  let findDuelists lifeWell =
    findDuel lifeWell
    <!> fun d -> d.Duelists
    
  let findDuelist color lifeWell =
    findDuelists lifeWell
    >>= List.tryFind (fun c -> c = color)
  
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
    >>= (Map.tryFind row)
    >>= Map.tryFind column
    
  let findPiece coords lifeWell =
    findTile coords lifeWell
    >>= fun t -> t.Piece
    
  let findUi lifeWell =
    lifeWell.Ui
    
  let findPopups lifeWell =
    lifeWell
    |> findUi
    |> fun ui -> ui.Popups
    
  let findPopupStates lifeWell =
    lifeWell
    |> findUi
    |> fun ui -> ui.PopupStates
    
  let findLoginPopupState lifeWell =
    lifeWell
    |> findPopupStates
    |> fun states -> states.LoginPopupState
