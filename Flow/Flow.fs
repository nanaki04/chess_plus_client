namespace ChessPlus

module Flow =
  open Well
  open Maelstrom.Mealstrom
  open Tides
  open WellLogger
  open WaveLogger
  open VisionQuest
  
  type Wave = ((string * string) * Maelstrom.Amplitude<Maelstrom.Amplitude<Waves.Amplitude>>)
  
  let (private doFlow, fetch, guard) = invoke LifeWell.initial tides [waveLogger; waveVisionQuest] [lifeWellLogger; lifeWellVisionQuest]
  let flowLifeWell wave =
    doFlow wave
    wave
    
  Fetchers.fetchLifeWell <- fetch
  
  let (private doFlowUiWell, fetchUiWell, guardUiWell) = invoke UiWell.initial uiTides [waveVisionQuest] [uiWellVisionQuest]
  let flowUi wave =
    doFlowUiWell wave
    wave
    
  Fetchers.fetchUiWell <- fetchUiWell
  
  let (private doFlowTileWell, fetchTileWell, guardTileWell) = invoke TileWell.initial tileTides [waveVisionQuest] [tileWellVisionQuest]
  let flowTile wave =
    doFlowTileWell wave
    wave
    
  Fetchers.fetchTileWell <- fetchTileWell
  
  let (private doFlowTileSelectionWell, fetchTileSelectionWell, guardTileSelectionWell) = invoke TileSelectionWell.initial tileSelectionTides [waveVisionQuest] [tileSelectionWellLogger; tileSelectionWellVisionQuest]
  let flowTileSelection wave =
    doFlowTileSelectionWell wave
    wave
    
  Fetchers.fetchTileSelectionWell <- fetchTileSelectionWell
  
  let (private doFlowPieceWell, fetchPieceWell, guardPieceWell) = invoke PieceWell.initial pieceTides [waveVisionQuest] [pieceWellLogger; pieceWellVisionQuest]
  let flowPiece wave =
    doFlowPieceWell wave
    wave  
    
  Fetchers.fetchPieceWell <- fetchPieceWell
  
  let (private doFlowRuleWell, fetchRuleWell, guardRuleWell) = invoke RuleWell.initial ruleTides [waveVisionQuest] [ruleWellVisionQuest]
  let flowRule wave =
    doFlowRuleWell wave
    wave
    
  Fetchers.fetchRuleWell <- fetchRuleWell
  
  let flow : (Wave -> Result<unit, string>) =
    flowLifeWell
    >> flowUi
    >> flowTile
    >> flowTileSelection
    >> flowPiece
    >> flowRule
    >> ignore
    >> Ok
  
  // TODO refactor  
  Conquers.init ()
  RuleApplication.init ()