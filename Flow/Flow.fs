namespace ChessPlus

module Flow =
  open Well
  open Maelstrom.Mealstrom
  open Tides
  open WellLogger
  open WaveLogger
  
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
  
  let (private doFlowTileSelectionWell, fetchTileSelectionWell, guardTileSelectionWell) = invoke TileSelectionWell.initial tileSelectionTides [] []
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
  
  let flow : (Wave -> Result<unit, string>) =
    flowLifeWell
    >> flowUi
    >> flowTile
    >> flowTileSelection
    >> flowPiece
    >> flowRule
    >> ignore
    >> Ok
    
//  let mutable private queue : (unit -> unit) list = []
//  
//  let rec private next () =  
//    if queue.Length > 0 then
//      let command = List.head queue
//      command ()
//      queue <- List.tail queue
//      next ()
//    else
//      Ok ()
//      
//  let enqueue command =
//    queue <- command::(List.rev queue) |> List.rev
//    if queue.Length = 1 then next () else Ok ()
//            
//  let flow wave =
//    let command = fun () ->
//      Logger.log "HANDLE WAVE"
//      Logger.log (JsonConversions.export (Moulds.LocationMould.export (Tuple.fst wave)))
//      maelstrom <- flow wave maelstrom
//      
//    enqueue command
//    
//  let guard wellGuardian =
//    let (unguard, mstr) = guard wellGuardian maelstrom
//    maelstrom <- mstr
//    fun () ->
//      maelstrom <- unguard maelstrom