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
    
  let findRules lifeWell =
    findDuel lifeWell
    <!> fun d -> d.Rules
    
  let findConnection lifeWell =
    lifeWell.Connection
    
  let findDuelists lifeWell =
    findDuel lifeWell
    <!> fun d -> d.Duelists
    
  let findDuelist color lifeWell =
    findDuelists lifeWell
    >>= List.tryFind (fun c -> c = color)
  
  let findBoard lifeWell =
    lifeWell
    |> findDuel
    |> Option.map (fun d -> d.Board)
    
  let findTiles lifeWell =
    lifeWell
    |> findBoard
    <!> fun b -> b.Tiles
    
  let findTile (row, column) lifeWell =
    findBoard lifeWell
    <!> fun b -> b.Tiles
    >>= (Map.tryFind row)
    >>= Map.tryFind column
          
  let findPiece coords lifeWell =
    findTile coords lifeWell
    >>= fun t -> t.Piece
    
  let findPieceRules coord well =
    let (<*>) = Option.apply
  
    let ruleIndices =
      findPiece coord well
      <!> Pieces.map (fun p -> p.Rules)
    
    let rules = findRules well
    
    Some (fun (indices : int list) (rules : Map<int, Rule>) ->
      List.map (fun i -> Map.tryFind i rules) indices
      |> Option.unwrap
    )
    <*> ruleIndices
    <*> rules
    |> Option.flatten
    
  let findPieceMovementRules coord well =
    let timer = Logger.time "findPieceMovementRules"
    findPieceRules coord well
    <!> List.filter (fun r ->
      match r with
      | MoveRule r -> true
      | _ -> false
    )
    |> timer
    
  let findPieceConquerRules coord well =
    findPieceRules coord well
    <!> List.filter (fun r ->
      match r with
      | ConquerRule r -> true
      | _ -> false
    )
    
  let findPieceMoveComboRules coord well =
    findPieceRules coord well
    <!> List.filter (fun r ->
      match r with
      | MoveComboRule r -> true
      | _ -> false
    )
    
  let findClientDuelist well =
    match findPlayer well, findDuelists well with
    | Some player, Some duelists ->
      List.tryFind (fun (d : Duelist) -> d.Name = player.Name) duelists
    | _, _ ->
      None
      
  let findSelectedTile well =
    let (<*>) = Option.apply
    
    let timer = Logger.time "findSelectedTile"
    (Some (fun (duelist : Duelist) tiles ->
      Matrix.firstRC (fun row column tile ->
        if tile.SelectedBy = Some duelist.Color
        then Some ((row, column), tile)
        else None
      ) tiles
    ))
    <*> findClientDuelist well
    <*> findTiles well
    |> Option.flatten
    |> timer
      
  let findTargetTile rule well =
    let findTarget (x, y) (row, column) =
      (Row.toInt row, Column.toInt column)
      |> fun (r, c) -> (r + x, c + y)
      |> fun (r, c) -> (Row.fromInt x, Column.fromInt c)
      |> function
        | (Ok row, Ok col) -> findTile (row, col) well
        | _ -> None
  
    match rule, (findSelectedTile well) with
    | MoveRule { Offset = offset; }, Some (coordinate, _) ->
      findTarget offset coordinate
    | ConquerRule { Offset = offset; }, Some(coordinate, _) ->
      findTarget offset coordinate
    | _, _ ->
      None
           
  let findTargetPiece rule well =
    findTargetTile rule well
    >>= fun tile -> tile.Piece      
              
  let findUi lifeWell =
    lifeWell.Ui
    
  let findPopups lifeWell =
    lifeWell
    |> findUi
    |> fun ui -> ui.Popups
    
  let findPopupStates lifeWell =
    lifeWell
    |> findUi
    |> fun ui -> ui.PopupStates
    
  let findLoginPopupState lifeWell =
    lifeWell
    |> findPopupStates
    |> fun states -> states.LoginPopupState
    
  let findUiComponents lifeWell =
    lifeWell
    |> findUi
    |> fun ui -> ui.Components
    
  let findUiComponent id lifeWell =
    lifeWell
    |> findUiComponents
    |> Map.tryFind id
