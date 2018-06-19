namespace ChessPlus

module Movements =
  open Finders
  open Fetchers
  open Updaters
  open Option
  open Types
  open RuleApplication
  
  let (<*>) = Option.apply
  
  let private areRuleConditionsMet rule tileSelectionWell =
    MoveRule.map (fun { Condition = conditions } ->
      Clauses.areMet conditions rule false tileSelectionWell
    ) rule
    |> Option.defaultValue (Ok false)
    
  let private filterSatisfiedRules rules tileSelectionWell =
    let (<*>) = Result.apply
  
    List.fold (fun acc rule ->
      match acc with
      | Ok r ->
        Ok (fun isSatisfied ->
          if isSatisfied
          then rule::r
          else r
        )
        <*> areRuleConditionsMet rule tileSelectionWell
      | err ->
        err
    ) (Ok List.empty) rules

  let calculateMovableTiles playerColor tileSelectionWell =
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
    <!> fun coord -> (fetchPieceMovementRules coord, findPiece coord pieceWell)
    |> function
    | Some (Some rules, piece) ->
      projectRules rules piece wellCollection
      |> getTileSelectionWell
    | _ ->
      Ok wellCollection
      |> getTileSelectionWell
   
  let resetMovableTiles playerColor tileSelectionWell =
    TileSelections.updateSelectionConquerable playerColor (fun _ -> List.empty) tileSelectionWell
   
  let updateMovableTiles playerColor tileSelectionWell =
    if Pool.isPlayer playerColor
    then calculateMovableTiles playerColor tileSelectionWell
    else tileSelectionWell
    
  let isMovableTile coord well =
    fetchOwnConquerableTileCoords ()
    |> List.contains coord