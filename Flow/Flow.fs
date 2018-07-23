namespace ChessPlus

module Flow =
  open Well
  open Maelstrom.Mealstrom
  open Tides
  open WellLogger
  open WaveLogger
  open VisionQuest
  
  type Wave = ((string * string) * Maelstrom.Amplitude<Maelstrom.Amplitude<Waves.Amplitude>>)
  
  let (private doFlow, fetch, guard) = invoke LifeWell.initial tides [waveLogger] [lifeWellLogger]
  let flowLifeWell wave =
    doFlow wave
    wave
    
  Fetchers.fetchLifeWell <- fetch
  
  let (private doFlowUiWell, fetchUiWell, guardUiWell) = invoke UiWell.initial uiTides [] []
  let flowUi wave =
    doFlowUiWell wave
    wave
    
  Fetchers.fetchUiWell <- fetchUiWell
  
  let (private doFlowTileWell, fetchTileWell, guardTileWell) = invoke TileWell.initial tileTides [] []
  let flowTile wave =
    doFlowTileWell wave
    wave
    
  Fetchers.fetchTileWell <- fetchTileWell
  
  let (private doFlowTileSelectionWell, fetchTileSelectionWell, guardTileSelectionWell) = invoke TileSelectionWell.initial tileSelectionTides [] [tileSelectionWellLogger]
  let flowTileSelection wave =
    doFlowTileSelectionWell wave
    wave
    
  Fetchers.fetchTileSelectionWell <- fetchTileSelectionWell
  
  let (private doFlowPieceWell, fetchPieceWell, guardPieceWell) = invoke PieceWell.initial pieceTides [] [pieceWellLogger]
  let flowPiece wave =
    doFlowPieceWell wave
    wave  
    
  Fetchers.fetchPieceWell <- fetchPieceWell
  
  let (private doFlowRuleWell, fetchRuleWell, guardRuleWell) = invoke RuleWell.initial ruleTides [] []
  let flowRule wave =
    doFlowRuleWell wave
    wave
    
  Fetchers.fetchRuleWell <- fetchRuleWell
  
  let private flowAll : (Wave -> unit) =
    flowLifeWell
    >> flowUi
    >> flowTile
    >> flowTileSelection
    >> flowPiece
    >> flowRule
    >> ignore
    
  let flow wave =
    flowAll wave
 
    // TODO development only   
    {
      LifeWell = fetch () |> Some;
      RuleWell = fetchRuleWell () |> Some;
      PieceWell = fetchPieceWell () |> Some;
      TileWell = fetchTileWell () |> Some;
      TileSelectionWell = fetchTileSelectionWell () |> Some;
      UiWell = fetchUiWell () |> Some;
    } 
    |> VisionQuest.report wave
    |> Ok  
  
  // TODO refactor  
  Conquers.init ()
  RuleApplication.init ()