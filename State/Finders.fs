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
    
  let findPlayerColor lifeWell =
    match findPlayer lifeWell, findDuelists lifeWell with
    | Some { Name = name }, Some duelists ->
      List.tryFind (fun (d : Duelist) -> d.Name = name) duelists
      <!> fun d -> d.Color
    | _, _ ->
      None
      
  let findOpponentColor lifeWell =
    match findPlayerColor lifeWell with
    | Some White -> Some Black
    | Some Black -> Some White
    | _ -> None
    
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
    
  let findKings pieceWell =
    Map.fold (fun kings _ piece ->
      match piece with
      | King k -> (King k)::kings
      | _ -> kings
    ) List.empty pieceWell
   
  let private findWhitePiece pieceList =
    List.find (fun p ->
      Types.Pieces.map (fun p -> p.Color) p
      |> (fun c -> c = White)
    ) pieceList
    
  let private findBlackPiece pieceList =
    List.find (fun p ->
      Types.Pieces.map (fun p -> p.Color) p
      |> (fun c -> c = Black)
    ) pieceList  
    
  let findWhiteKing : (PieceWell -> Pieces) =
    findKings >> findWhitePiece
    
  let findBlackKing : (PieceWell -> Pieces) =
    findKings >> findBlackPiece
    
  let findWhiteKingCoord pieceWell =
    findWhiteKing pieceWell
    |> Types.Pieces.map (fun p -> p.Coordinate)
    
  let findBlackKingCoord pieceWell =
    findBlackKing pieceWell
    |> Types.Pieces.map (fun p -> p.Coordinate)
    
  let findPiecesByColor color pieceWell =
    Map.fold (fun whitePieces _ piece ->
      if Types.Pieces.map (fun p -> p.Color = color) piece
      then piece::whitePieces
      else whitePieces
    ) List.empty pieceWell
    
  let findWhitePieces : (PieceWell -> Pieces list) =
    findPiecesByColor White
    
  let findBlackPieces : (PieceWell -> Pieces list) =
    findPiecesByColor Black
        
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
    
  let findOwnSelectedTileCoords tileSelectionWell lifeWell =
    findPlayerColor lifeWell
    >>= (fun c -> findSelectedTileCoord c tileSelectionWell)
    
  let findOwnSelectedPiece tileSelectionWell pieceWell lifeWell =
    findOwnSelectedTileCoords tileSelectionWell lifeWell
    >>= (fun c -> findPiece c pieceWell)
