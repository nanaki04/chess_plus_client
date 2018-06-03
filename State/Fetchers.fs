namespace ChessPlus

module Fetchers =
  open Well
  open Finders
  open Option
  
  let mutable fetchLifeWell = fun () -> LifeWell.initial
  
  let mutable fetchUiWell = fun () -> UiWell.initial
  
  let mutable fetchTileWell = fun () -> TileWell.initial
  
  let mutable fetchTileSelectionWell = fun () -> TileSelectionWell.initial
  
  let mutable fetchPieceWell = fun () -> PieceWell.initial
  
  let mutable fetchRuleWell = fun () -> RuleWell.initial
  
  let fetchPlayer () =
    fetchLifeWell ()
    |> findPlayer
    
  let fetchDuel () =
    fetchLifeWell ()
    |> findDuel

  let fetchDuelists () =
    fetchLifeWell ()
    |> findDuelists
    
  let fetchWhiteSelections () =
    fetchTileSelectionWell ()
    |> findWhiteSelections
    
  let fetchBlackSelections () =
    fetchTileSelectionWell ()
    |> findBlackSelections
    
  let fetchSelectionsForPlayer playerColor =
    match playerColor with
    | White -> fetchWhiteSelections ()
    | Black -> fetchBlackSelections ()
    
  let fetchSelectedTileCoord playerColor =
    fetchTileSelectionWell ()
    |> findSelectedTileCoord playerColor
    
  let fetchConquerableTileCoords playerColor =
    fetchTileSelectionWell ()
    |> findConquerableTileCoords playerColor
    
  let fetchTile coord =
    fetchTileWell ()
    |> findTile coord
    
  let fetchPiece coord =
    fetchPieceWell ()
    |> findPiece coord
    
  let fetchPieceRuleIDs coord =
    fetchPieceWell ()
    |> findPieceRuleIDs coord
    
  let fetchPieceRules coord =
    let rules = fetchRuleWell ()
    fetchPieceRuleIDs coord
    <!> List.map (fun k -> Map.tryFind k rules)
    >>= Option.unwrap
    
  let fetchPieceMovementRules coord =
    fetchPieceRules coord
    <!> List.filter (fun r ->
      match r with
      | MoveRule _ -> true
      | _ -> false
    )
    
  let fetchPieceConquerRules coord =
    fetchPieceRules coord
    <!> List.filter (fun r ->
      match r with
      | ConquerRule _ -> true
      | _ -> false
    )
    
  let fetchPieceMoveComboRules coord =
    fetchPieceRules coord
    <!> List.filter (fun r ->
      match r with
      | MoveComboRule _ -> true
      | _ -> false
    )
    
  let fetchClientDuelist () =
    match fetchPlayer (), fetchDuelists () with
    | Some { Name = name }, Some duelists ->
      List.tryFind (fun (d : Duelist) -> d.Name = name) duelists
    | x, y ->
      Logger.warn "fetchClientDuelist ERROR"
      Logger.warn x
      Logger.warn y
      None
      
  let fetchOpponentDuelist () =
    match fetchPlayer (), fetchDuelists () with
    | Some { Name = name }, Some duelists ->
      List.tryFind (fun (d : Duelist) -> d.Name <> name) duelists
    | _, _ ->
      None
      
  let fetchOwnSelectedTileCoords () =
    match fetchClientDuelist () with
    | Some { Color = color } ->
      fetchSelectedTileCoord color
    | _ ->
      None
      
  let fetchOpponentSelectedTileCoords () =
    match fetchOpponentDuelist () with
    | Some { Color = color } ->
      fetchSelectedTileCoord color
    | _ ->
      None
      
  let fetchOwnSelectedTile () =
    match fetchOwnSelectedTileCoords () with
    | Some coord ->
      fetchTile coord
    | None ->
      None
      
  let fetchOpponentSelectedTile () =
    match fetchOpponentSelectedTileCoords () with
    | Some coord ->
      fetchTile coord
    | None ->
      None
      
  let fetchOwnSelectedPiece () =
    fetchOwnSelectedTileCoords ()
    >>= fetchPiece
    
  let fetchOwnConquerableTileCoords () =
    match fetchClientDuelist () with
    | Some { Color = color } ->
      fetchConquerableTileCoords color
    | None ->
      List.empty
      
  let fetchOpponentConquerableTileCoords () =
    match fetchOpponentDuelist () with
    | Some { Color = color } ->
      fetchConquerableTileCoords color
    | None ->
      List.empty