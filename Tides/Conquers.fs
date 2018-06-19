namespace ChessPlus

module Conquers =
  open Finders
  open Fetchers
  open Updaters
  open Option
  open Types
  open RuleApplication
  
  let (<*>) = Option.apply
  
  let private areRuleConditionsMet rule isSimulation tileSelectionWell =
    ConquerRule.map (fun { Condition = conditions } ->
      Clauses.areMet conditions rule isSimulation tileSelectionWell
    ) rule
    |> Option.defaultValue (Ok false)
    
  let private filterSatisfiedRules (rules : Rule list) isSimulation tileSelectionWell =
    let (<*>) = Result.apply
  
    List.fold (fun acc rule ->
      match acc with
      | Ok r ->
        Ok (fun isSatisfied ->
          if isSatisfied
          then rule::r
          else r
        )
        <*> areRuleConditionsMet rule isSimulation tileSelectionWell
      | err ->
        err
    ) (Ok List.empty) rules

  let calculateConquerableTiles playerColor isSimulation tileSelectionWell =
    let pieceWell = fetchPieceWell ()
    let wellCollection =
      { Well.WellCollection.initial with
          TileSelectionWell = Some tileSelectionWell;
          PieceWell = Some pieceWell;
      }
      
    let getTileSelectionWell wellCollection =
      match wellCollection with
      | Ok { TileSelectionWell = Some well } -> well
      | _ -> tileSelectionWell
  
    findSelectedTileCoord playerColor tileSelectionWell
    >>= fun coord -> if Pool.Pieces.isPlayerPiece coord pieceWell then Some coord else None
    <!> fun coord -> (fetchPieceConquerRules coord, findPiece coord pieceWell)
    |> function
    | Some (Some rules, piece) ->
      projectRules rules piece wellCollection
      |> getTileSelectionWell
    | _ ->
      Ok wellCollection
      |> getTileSelectionWell
   
  let updateConquerableTiles playerColor isSimulation tileSelectionWell =
    if Pool.isPlayer playerColor
    then calculateConquerableTiles playerColor isSimulation tileSelectionWell
    else tileSelectionWell
        
  let canConquer coord isSimulation piece =
    piece.Coordinate
    >>= (fun pieceCoord -> 
      let offset = Types.Coordinate.getOffset pieceCoord coord
      fetchPieceConquerRules pieceCoord
      <!> (fun rules -> filterSatisfiedRules rules isSimulation (fetchTileSelectionWell ()))
      <!> Result.map List.length
    )
    |> function
    | Some (Ok 0) -> false
    | Some (Ok _) -> true
    | _ -> false
    
  let canAnyConquer coord pieces =
    List.fold (fun conquerable piece ->
      if conquerable
      then true
      else Types.Pieces.map (canConquer coord true) piece
    ) false pieces    
    
  let canConquerBlackKing pieceWell =
    findBlackKingCoord pieceWell
    <!> (fun kingCoord -> canAnyConquer kingCoord (findWhitePieces pieceWell))
    |> Option.defaultValue false

  let canConquerWhiteKing pieceWell =
    findWhiteKingCoord pieceWell
    <!> (fun kingCoord -> canAnyConquer kingCoord (findBlackPieces pieceWell))
    |> Option.defaultValue false
    
  ConditionVerification.canConquerBlackKing <- canConquerBlackKing
  ConditionVerification.canConquerWhiteKing <- canConquerWhiteKing