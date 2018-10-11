namespace ChessPlus

module Conquers =
  open Finders
  open Fetchers
  open Updaters
  open Option
  open Types
  open RuleApplication
  
  let (<*>) = Option.apply
  
  let private areRuleConditionsMet rule piece isSimulation wellCollection =
    match rule with
    | ConquerRule { Condition = conditions } ->
      Clauses.areMet conditions rule piece isSimulation wellCollection
    | ConquerComboRule { Condition = conditions } ->
      Clauses.areMet conditions rule piece isSimulation wellCollection
    | _ ->
      Ok false
    
  let private filterSatisfiedRules (rules : Rule list) piece isSimulation wellCollection =
    let (<*>) = Result.apply
  
    List.fold (fun acc rule ->
      match acc with
      | Ok r ->
        Ok (fun isSatisfied ->
          if isSatisfied
          then rule::r
          else r
        )
        <*> areRuleConditionsMet rule piece isSimulation wellCollection
      | err ->
        err
    ) (Ok List.empty) rules

  let calculateConquerableTiles playerColor isSimulation wellCollection =
    match wellCollection with
    | { TileSelectionWell = Some tileSelectionWell; PieceWell = Some pieceWell } ->
      findSelectedTileCoord playerColor tileSelectionWell
      >>= fun coord -> if Pool.Pieces.isPlayerPiece coord pieceWell then Some coord else None
      <!> (fun coord ->
        (findPieceConquerAndComboRules
          coord
          (fetchRuleWell ())
          pieceWell
          (fetchBuffWell ())
          , findPiece coord pieceWell
        )
      )
      |> function
      | Some (Some rules, piece) ->
        filterSatisfiedRules rules piece isSimulation wellCollection
        |> Result.bind (fun r -> projectRules r piece wellCollection)
      | _ ->
        Ok wellCollection
    | _ ->
      Ok wellCollection
   
  let updateConquerableTiles playerColor isSimulation tileSelectionWell =
    let lifeWell = fetchLifeWell ()
    let wellCollection = {
      Well.WellCollection.initial with
        TileSelectionWell = Some tileSelectionWell;
        PieceWell = Some (fetchPieceWell ());
        LifeWell = Some lifeWell;
    }
    
    match Pool.isPlayer playerColor, Pool.isPlayerTurn lifeWell with
    | true, true ->
      calculateConquerableTiles playerColor isSimulation wellCollection
      |> function
      | Ok { TileSelectionWell = Some tw } -> Ok tw
      | Ok _ -> Ok tileSelectionWell
      | Error e -> Error e
    | _, _ ->
      Ok tileSelectionWell
        
  let canConquer coord isSimulation (piece : Pieces) wellCollection =
    match wellCollection with
    | { PieceWell = Some pieceWell } ->
      Types.Pieces.coord piece
      >>= (fun pieceCoord -> 
        let offset = Types.Coordinate.getOffset pieceCoord coord
        findPieceConquerAndComboRules pieceCoord (fetchRuleWell ()) pieceWell (fetchBuffWell ())
        <!> List.filter (fun rule ->
          match rule with
          | ConquerRule { Offset = ruleOffset } -> ruleOffset = offset
          | _ -> false      
        )
        <!> (fun rules -> filterSatisfiedRules rules (Some piece) isSimulation wellCollection)
        <!> Result.map List.length
      )
      |> function
      | Some (Ok 0) ->
        false
      | Some (Ok _) ->
        true
      | _ ->
        false
    | _ ->
      false
    
  let canAnyConquer coord (pieces : Pieces list) wellCollection =
    List.fold (fun conquerable piece ->
      if conquerable
      then true
      else canConquer coord true piece wellCollection
    ) false pieces    
    
  let canAnyConquerPiece (piece : Pieces) wellCollection =
    match wellCollection, Types.Pieces.color piece, Types.Pieces.coord piece with
    | { PieceWell = Some pieceWell }, White, Some coord ->
      canAnyConquer coord (findBlackPieces pieceWell) wellCollection
    | { PieceWell = Some pieceWell }, Black, Some coord ->
      canAnyConquer coord (findWhitePieces pieceWell) wellCollection
    | _, _, _ ->
      false
         
  let canConquerBlackKing wellCollection =
    match wellCollection with
    | { PieceWell = Some pieceWell } ->
      findBlackKingCoord pieceWell
      <!> (fun kingCoord -> canAnyConquer kingCoord (findWhitePieces pieceWell) wellCollection)
      |> Option.defaultValue false
    | _ -> false
    
  let canConquerWhiteKing wellCollection =
    match wellCollection with
    | { PieceWell = Some pieceWell } ->
      findWhiteKingCoord pieceWell
      <!> (fun kingCoord -> canAnyConquer kingCoord (findBlackPieces pieceWell) wellCollection)
      |> Option.defaultValue false
    | _ -> false
    
  // TODO refactor  
  let init () =
    ConditionVerification.canConquerBlackKing <- canConquerBlackKing
    ConditionVerification.canConquerWhiteKing <- canConquerWhiteKing
    ConditionVerification.canAnyConquerPiece <- canAnyConquerPiece