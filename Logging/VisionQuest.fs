namespace ChessPlus

module VisionQuest =
  open Well
  open JsonConversions
  open Moulds

  type private Packet () =
    let mutable domain = Unchecked.defaultof<string>
    let mutable invocation = Unchecked.defaultof<string>
    let mutable payload = Unchecked.defaultof<string>
    
    member m.Domain
      with get () = domain
      and set (v) = domain <- v
    member m.Invocation
      with get () = invocation
      and set (v) = invocation <- v
    member m.Payload
      with get () = payload
      and set (v) = payload <- v
      
  type private HistoryItemDto () =
    let mutable domain = Unchecked.defaultof<string>
    let mutable invocation = Unchecked.defaultof<string>
    let mutable amplitude = Unchecked.defaultof<string>
    let mutable stateType = Unchecked.defaultof<string>
    let mutable state = Unchecked.defaultof<string>
    
    member m.Domain
      with get () = domain
      and set (v) = domain <- v
    member m.Invocation
      with get () = invocation
      and set (v) = invocation <- v
    member m.Amplitude
      with get () = amplitude
      and set (v) = amplitude <- v
    member m.StateType
      with get () = stateType
      and set (v) = stateType <- v
    member m.State
      with get () = state
      and set (v) = state <- v
              
  let private call = Agent.start (fun () -> None)
  
  let private sendItem item =
    let packet = new Packet ()
    packet.Domain <- "item"
    packet.Invocation <- "add"
    packet.Payload <- item |> export
    
    packet
    |> export
    |> (fun p -> p + "\n")
    |> VisionQuestTcp.send
    
    None
    
  let private reportWave wave =
    let ((domain, invocation), _) = wave
    call (fun historyItem ->
      match historyItem, exportMould wave with
      | None, Ok mould ->
        let historyItem = new HistoryItemDto ()
        historyItem.Domain <- domain
        historyItem.Invocation <- invocation
        historyItem.Amplitude <- mould
        Some historyItem
      | Some item, _ ->
        Logger.error ("VisionQuest: log data lost for: " + item.Domain + ": " + item.Invocation)
        None
      | _, _ ->
        Logger.error ("VisionQuest: unable to log wave " + domain + ":" + invocation)
        None
    )
    
  let private reportWellMovement well =
    call (fun historyItem ->
      match historyItem, well with
      | Some historyItem, LifeWell w ->
        historyItem.StateType <- "LifeWell"
        historyItem.State <- LifeWellDto.export w |> export
        sendItem historyItem
      | Some historyItem, RuleWell w ->
        historyItem.StateType <- "RuleWell"
        historyItem.State <- RuleWellDto.export w |> export
        sendItem historyItem
      | Some historyItem, PieceWell w ->
        historyItem.StateType <- "PieceWell"
        historyItem.State <- PieceWellDto.export w |> export
        sendItem historyItem
      | Some historyItem, TileSelectionWell w ->
        historyItem.StateType <- "TileSelectionWell"
        historyItem.State <- TileSelectionWellDto.export w |> export
        sendItem historyItem
      | Some historyItem, TileWell w ->
        historyItem.StateType <- "TileWell"
        historyItem.State <- TileWellDto.export w |> export
        sendItem historyItem
      | Some historyItem, UiWell w ->
        historyItem.StateType <- "UiWell"
        historyItem.State <- UiWellDto.export w |> export
        sendItem historyItem
      | None, _ ->
        Logger.error ("VisionQuest: well data lost, as no wave was reported first")
        None
    )
    
  let lifeWellVisionQuest next well =
    let refreshedWell = next well
    reportWellMovement (LifeWell refreshedWell)
    refreshedWell
    
  let ruleWellVisionQuest next well =
    let refreshedWell = next well
    reportWellMovement (RuleWell refreshedWell)
    refreshedWell
    
  let pieceWellVisionQuest next well =
    let refreshedWell = next well
    reportWellMovement (PieceWell refreshedWell)
    refreshedWell
   
  let tileSelectionWellVisionQuest next well =
    let refreshedWell = next well
    reportWellMovement (TileSelectionWell refreshedWell)
    refreshedWell   
 
  let tileWellVisionQuest next well =
    let refreshedWell = next well
    reportWellMovement (TileWell refreshedWell)
    refreshedWell  
    
  let uiWellVisionQuest next well =
    let refreshedWell = next well
    reportWellMovement (UiWell refreshedWell)
    refreshedWell   
          
  let waveVisionQuest next wave =
    reportWave wave
    next wave
    
  VisionQuestTcp.listen ()