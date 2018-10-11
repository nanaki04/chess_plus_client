namespace ChessPlus

module RuleApplication =
  open Finders
  open Fetchers
  open Updaters
  open Option
  open Types
  
  let (<*>) = Option.apply

  let applyRule rule piece wellCollection =
    match rule, piece, wellCollection with
    | MoveRule { Offset = offset }, Some p, { PieceWell = Some pieceWell } ->
      Pieces.map (fun p -> p.Coordinate) p
      <!> (Coordinate.applyOffset offset)
      |> function
      | Some (Ok coord) ->
        Pool.Pieces.movePiece p coord pieceWell
        |> fun w -> Ok { wellCollection with PieceWell = Some w }
      | _ ->
        Ok wellCollection
        
    | ConquerRule { Offset = offset }, Some p, { PieceWell = Some pieceWell } ->
      Pieces.map (fun p -> p.Coordinate) p
      <!> (Coordinate.applyOffset offset)
      |> function
      | Some (Ok coord) ->
        Pool.Pieces.movePiece p coord pieceWell
        |> fun w -> Ok { wellCollection with PieceWell = Some w }
      | _ ->
        Ok wellCollection
        
    | _, _, _ ->
      Error "Rule could not be applied"
      
  let applyRules rules piece wellCollection =
    List.fold (fun well rule ->
      Result.bind (applyRule rule piece) well
    ) (Ok wellCollection) rules
    
  let projectRule rule piece wellCollection =
    match rule, piece, wellCollection with
    | MoveRule { Offset = offset }, Some p, { TileSelectionWell = Some well } ->
      Pieces.map (fun p -> (p.Coordinate, p.Color)) p
      |> function
      | (Some coord, color) ->
        Coordinate.applyOffset offset coord
        |> Result.toOption
        <!> (fun coord ->
          TileSelections.updateSelectionConquerable color (fun conquerable -> coord::conquerable) well
        )
        <!> fun well -> Ok { wellCollection with TileSelectionWell = Some well; }
        |> Option.defaultValue (Ok wellCollection)
      | _ ->
        Ok wellCollection
        
    | ConquerRule { Offset = offset }, Some p, { TileSelectionWell = Some well } ->
      Pieces.map (fun p -> (p.Coordinate, p.Color)) p
      |> function
      | (Some coord, color) ->
        Coordinate.applyOffset offset coord
        |> Result.toOption
        <!> (fun coord ->
          TileSelections.updateSelectionConquerable color (fun conquerable -> coord::conquerable) well
        )
        <!> fun well -> Ok { wellCollection with TileSelectionWell = Some well; }
        |> Option.defaultValue (Ok wellCollection)
      | _ ->
        Ok wellCollection           
     
    | MoveComboRule { MyOffset = offset; }, Some p, { TileSelectionWell = Some well } ->
      Logger.log "simulate MoveComboRule"
      Pieces.map (fun p -> (p.Coordinate, p.Color)) p
      |> function
      | (Some coord, color) ->
        Coordinate.applyOffset offset coord
        |> Result.toOption
        <!> (fun coord ->
          TileSelections.updateSelectionConquerable color (fun conquerable -> coord::conquerable) well
        )
        <!> fun well -> Ok { wellCollection with TileSelectionWell = Some well; }
        |> Option.defaultValue (Ok wellCollection)
      | _ ->
        Ok wellCollection
        
    | ConquerComboRule { MyMovement = offset }, Some p, { TileSelectionWell = Some well } ->
      Pieces.map (fun p -> (p.Coordinate, p.Color)) p
      |> function
      | (Some coord, color) ->
        Coordinate.applyOffset offset coord
        |> Result.toOption
        <!> (fun coord ->
          TileSelections.updateSelectionConquerable color (fun conquerable -> coord::conquerable) well
        )
        <!> fun well -> Ok { wellCollection with TileSelectionWell = Some well; }
        |> Option.defaultValue (Ok wellCollection)
      | _ ->
        Ok wellCollection       
        
    | _, _, _ ->
      Error "Rule could not be projected"

  let projectRules rules piece wellCollection =
    List.fold (fun well rule ->
      Result.bind (projectRule rule piece) well
    ) (Ok wellCollection) rules     
    
  // TODO refactor
  let init () =
    ConditionVerification.applyRule <- applyRule 