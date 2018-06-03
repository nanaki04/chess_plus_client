namespace ChessPlus

module Finders =
  open Well
  open Maelstrom.WellGuardians
  open Option
  open Types
  
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
    
  let findWhiteSelections well =
    well.White
    
  let findBlackSelections well =
    well.Black 
  
  let findSelectionsForPlayer playerColor well =
    match playerColor with
    | White -> findWhiteSelections well
    | Black -> findBlackSelections well
       
  let findSelectedTileCoord playerColor well =
    findSelectionsForPlayer playerColor well
    |> fun s -> s.Selected
    
  let findConquerableTileCoords playerColor well =
    findSelectionsForPlayer playerColor well
    |> fun s -> s.Conquerable
    
  let findTile coord well =
    Map.tryFind coord well
          
  let findPiece coord well =
    Map.tryFind coord well
    
  let findPieceById id well =
    Map.first (fun k v -> Types.Pieces.map (fun p -> p.ID = id) v) well
    
  let findPieceRuleIDs coord well =
    findPiece coord well
    <!> Types.Pieces.map (fun p -> p.Rules)   
    
  let findPopups well =
    well.Popups
    
  let findPopupStates well =
    well.PopupStates
    
  let findLoginPopupState well =
    well
    |> findPopupStates
    |> fun states -> states.LoginPopupState
    
  let findUiComponents well =
    well.Components
    
  let findUiComponent id well =
    well
    |> findUiComponents
    |> Map.tryFind id
