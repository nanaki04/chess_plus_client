﻿namespace ChessPlus

module Movements =
  open Finders
  open Fetchers
  open Updaters
  open Option
  open Types
  open RuleApplication
  
  let (<*>) = Option.apply
  
  let private areRuleConditionsMet rule piece wellCollection =
    MoveRule.map (fun { Condition = conditions } ->
      Clauses.areMet conditions rule piece false wellCollection
    ) rule
    |> Option.defaultValue (Ok false)
    
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
      <!> fun coord -> (findPieceMovementRules coord (fetchRuleWell ()) pieceWell, findPiece coord pieceWell)
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
    let wellCollection t = {
      Well.WellCollection.initial with
        TileSelectionWell = Some t;
        PieceWell = Some (fetchPieceWell ());
        LifeWell = Some (fetchLifeWell ());
    }
    
    if Pool.isPlayer playerColor
    then
      resetMovableTiles playerColor tileSelectionWell
      |> wellCollection
      |> calculateMovableTiles playerColor
      |> function
      | Ok { TileSelectionWell = Some tw } -> Ok tw
      | Ok _ -> Ok tileSelectionWell
      | Error e -> Error e
    else
      Ok tileSelectionWell
    
  let isMovableTile coord well =
    fetchOwnConquerableTileCoords ()
    |> List.contains coord