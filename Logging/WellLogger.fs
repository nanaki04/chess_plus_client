namespace ChessPlus

module WellLogger =
  open UnityEngine
  open Logger
  open JsonConversions
  open Well
  
  let private logWell well =
    match well with
    | LifeWell w ->
      LifeWellDto.export w
      |> export
      |> log
    | RuleWell w ->
      ()
    | PieceWell w ->
      PieceWellDto.export w
      |> export
      |> log
    | TileSelectionWell w ->
      TileSelectionWellDto.export w
      |> export
      |> log
    | TileWell w ->
      ()
    | UiWell w ->
      ()
  
  let wellLogger before after = 
    let unionName = Logger.getUnionName before
    log("<color='blue'>" + unionName + " BEFORE</color>")
    logWell before
    log("<color='green'>" + unionName + " AFTER</color>")
    logWell after
    
  let lifeWellLogger next well =
    let refreshedWell = next well
    wellLogger (LifeWell well) (LifeWell refreshedWell)
    refreshedWell
    
  let pieceWellLogger next well =
    let refreshedWell = next well
    wellLogger (PieceWell well) (PieceWell refreshedWell)
    refreshedWell
    
  let tileSelectionWellLogger next well =
    let refreshedWell = next well
    wellLogger (TileSelectionWell well) (TileSelectionWell refreshedWell)
    refreshedWell