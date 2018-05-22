namespace ChessPlus

module Movements =
  open Finders
  open Updaters
  open Option
  open Types
  
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

  let movableTiles well =
    let timer = Logger.time "movableTiles"
    findSelectedTile well
    >>= fun (coord, _) -> findPieceMovementRules coord well <!> (Tuple.retn2 coord)
    <!> fun (coord, rules) -> (coord, filterSatisfiedRules rules well)
    <!> fun ((r, c), rules) -> ((Row.toInt r, Column.toInt c), rules)
    <!> (fun ((r, c), rules) ->
      rules
      |> Result.map (List.map (MoveRule.map (fun { Offset = (x, y) } ->
        (r + x, c + y)
      )))
      |> Result.map (List.map (Option.defaultValue (0, 0)))
      |> Result.map (fun coordinates -> List.map (Coordinate.fromInt >> Result.toOption) coordinates)
    )
    |> Option.defaultValue (Ok List.empty)
    |> timer
   
  let updateMovableTiles playerColor well =
    let timer = Logger.time "updateMovableTiles"
    match movableTiles well, findTiles well with
    | Ok movableTiles, Some tiles ->
      Matrix.updateWhere 
        (fun tile -> tile.ConquerableBy = Some playerColor)
        (fun tile -> { tile with ConquerableBy = None })
      >> (fun tiles ->
        List.fold (fun (m : Map<Row, Map<Column, Tile>>) coord ->
          Option.map (fun (r, c) ->
            Matrix.update r c (Option.map (fun tile -> { tile with ConquerableBy = Some playerColor})) m
          ) coord
          |> Option.defaultValue m
        ) tiles movableTiles
      )
      |> updateTiles <| well
      |> timer
    | Error e, _ ->
      Logger.warn e
      well
    | _, _ ->
      Logger.warn "no movable tiles"
      well
    
  let isMovableTile coord well =
    let (<!>) = fun f v -> Result.map v f
    
    movableTiles well
    <!> List.tryFind (fun movableTile -> movableTile = coord)
    <!> function
      | Some _ -> true
      | _ -> false
    