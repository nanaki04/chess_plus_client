namespace ChessPlus

module Movements =
  open Finders
  open Fetchers
  open Updaters
  open Option
  open Types
  open RuleApplication
  
  let (<*>) = Option.apply
  
  let private areRuleConditionsMet rule piece wellCollection =
    match rule with
    | MoveRule { Condition = conditions } ->
      Clauses.areMet conditions rule piece false wellCollection
    | MoveComboRule { Condition = conditions } ->
      Clauses.areMet conditions rule piece false wellCollection
    | _ ->
      Ok false
    
  let private filterSatisfiedRules rules piece wellCollection =
    let (<*>) = Result.apply
  
    List.fold (fun acc rule ->
      match acc with
      | Ok r ->
        Ok (fun isSatisfied ->
          if isSatisfied
          then rule::r
          else r
        )
        <*> areRuleConditionsMet rule piece wellCollection
      | err ->
        err
    ) (Ok List.empty) rules

  let calculateMovableTiles playerColor wellCollection =
    match wellCollection with
    | { TileSelectionWell = Some tileSelectionWell; PieceWell = Some pieceWell } ->
      findSelectedTileCoord playerColor tileSelectionWell
      >>= fun coord -> if Pool.Pieces.isPlayerPiece coord pieceWell then Some coord else None
      <!> (fun coord ->
        (findPieceMovementAndComboRules
          coord
          (fetchRuleWell ())
          pieceWell
          (fetchBuffWell ())
          , findPiece coord pieceWell
        )
      )
      |> function
      | Some (Some rules, piece) ->
        filterSatisfiedRules rules piece wellCollection
        |> Result.bind (fun r -> projectRules r piece wellCollection)
      | _ ->
        Ok wellCollection
    | _ ->
      Ok wellCollection
   
  let resetMovableTiles playerColor tileSelectionWell =
    TileSelections.updateSelectionConquerable playerColor (fun _ -> List.empty) tileSelectionWell
   
  let updateMovableTiles playerColor tileSelectionWell =
    let lifeWell = fetchLifeWell ()
    let wellCollection = {
      Well.WellCollection.initial with
        TileSelectionWell = Some (resetMovableTiles playerColor tileSelectionWell);
        PieceWell = Some (fetchPieceWell ());
        LifeWell = Some lifeWell;
    }
    
    match Pool.isPlayer playerColor, Pool.isPlayerTurn lifeWell with
    | true, true ->
      calculateMovableTiles playerColor wellCollection
      |> function
      | Ok { TileSelectionWell = Some tw } -> Ok tw
      | Ok _ -> Ok tileSelectionWell
      | Error e -> Error e
    | _, _ ->
      Ok tileSelectionWell
    
  let isMovableTile coord well =
    fetchOwnConquerableTileCoords ()
    |> List.contains coord