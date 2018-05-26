namespace ChessPlus

module Movements =
  open Finders
  open Updaters
  open Option
  open Types
  
  let (<*>) = Option.apply
  
  let private areRuleConditionsMet rule well =
    let timer = Logger.time "areRuleConditionsMet"
    MoveRule.map (fun { Condition = conditions } ->
      Clauses.areMet conditions rule well
    ) rule
    |> Option.defaultValue (Ok false)
    |> timer
    
  let private filterSatisfiedRules rules well =
    Ok rules
//    let (<*>) = Result.apply
//    
//    let timer = Logger.time "filterSatisfiedRules"
//    List.fold (fun acc rule ->
//      match acc with
//      | Ok rules ->
//        Ok (fun isSatisfied -> rule::rules)
//        <*> areRuleConditionsMet rule well
//      | err ->
//        err
//    ) (Ok List.empty) rules
//    |> timer

  let calculateMovableTiles playerColor well =
    findSelectedTileCoord playerColor well
    >>= fun coord -> findPieceMovementRules coord well <!> (Tuple.retn2 coord)
    <!> fun (coord, rules) -> (coord, filterSatisfiedRules rules well)
    <!> fun ((r, c), rules) -> ((Row.toInt r, Column.toInt c), rules)
    <!> (fun ((r, c), rules) ->
      rules
      |> Result.map (List.map (MoveRule.map (fun { Offset = (x, y) } ->
        (r + x, c + y)
      )))
      |> Result.map (List.map (Option.defaultValue (0, 0)))
      |> Result.map (fun coordinates -> List.map (Coordinate.fromInt >> Result.toOption) coordinates)
      |> Result.map Option.filter
    )
    |> Option.defaultValue (Ok List.empty)
   
  let resetMovableTiles playerColor well =
    updateConquerableTiles playerColor (fun t -> { t with ConquerableBy = None }) well
  
  let setMovableTiles playerColor well =
    findConquerableTileCoords playerColor well
    <!> (fun coords ->
      updateTiles (fun tiles ->
        Matrix.updateAll coords (fun _ tile ->
          Option.map (fun t -> { t with ConquerableBy = Some playerColor }) tile
        ) tiles
      ) well
    )
    |> Option.defaultValue well
   
  let updateMovableTiles playerColor well =
    Logger.now "updateMovableTiles:calculateMovableTiles"
    match calculateMovableTiles playerColor well with
    | Ok coords ->
      Logger.now "updateMovableTiles:resetMovableTiles"
      resetMovableTiles playerColor well
      |> Logger.nowP "updateMovableTiles:updateSelectionConquerable"
      |> updateSelectionConquerable playerColor (fun _ -> coords)
      |> Logger.nowP "updateMovableTiles:setMovableTiles"
      |> setMovableTiles playerColor
      |> Logger.nowP "updateMovableTiles:done"
    | Error e ->
      Logger.warn e
      well
    
  let isMovableTile coord well =
    findOwnConquerableTileCoords well
    <!> List.contains coord
    |> Option.defaultValue false
    