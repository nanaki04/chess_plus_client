namespace ChessPlus

module VisionQuest =
  open Well
  open JsonConversions
  open Moulds
      
  type HistoryItemDto () =
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
      
  type PayloadDto () =
    let mutable historyItem = Unchecked.defaultof<HistoryItemDto>
    let mutable json = Unchecked.defaultof<string>
    
    member m.HistoryItem
      with get () = historyItem
      and set (v) = historyItem <- v
    member m.Json
      with get () = json
      and set (v) = json <- v

  type Packet () =
    let mutable domain = Unchecked.defaultof<string>
    let mutable invocation = Unchecked.defaultof<string>
    let mutable payload = Unchecked.defaultof<PayloadDto>
    
    member m.Domain
      with get () = domain
      and set (v) = domain <- v
    member m.Invocation
      with get () = invocation
      and set (v) = invocation <- v
    member m.Payload
      with get () = payload
      and set (v) = payload <- v
              
  let private call = Agent.start (fun () -> None)
  
  let private sendItem item =
    let payload = new PayloadDto ()
    payload.HistoryItem <- item
  
    let packet = new Packet ()
    packet.Domain <- "item"
    packet.Invocation <- "add"
    packet.Payload <- payload
    
    packet
    |> export
    |> Logger.inspect "VISION QUEST PACKET"
    |> VisionQuestTcp.send
    
    None
    
  let private sendQuitSignal () =
    let packet = new Packet ()
    packet.Domain <- "client"
    packet.Invocation <- "remove"
    packet.Payload <- new PayloadDto ()
    packet.Payload.Json <- ""
    
    packet
    |> export
    |> VisionQuestTcp.send
    
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
        Logger.error ("VisionQuest: log data lost for: " + item.Domain + ":" + item.Invocation)
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
      | Some historyItem, BuffWell w ->
        historyItem.StateType <- "BuffWell"
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
    
  let private reportWellCollection wave wellCollection =
    let historyItem = new HistoryItemDto ()
    match wave, exportMould wave with
    | ((domain, invocation), _), Ok mould ->
      historyItem.Domain <- domain
      historyItem.Invocation <- invocation
      historyItem.Amplitude <- mould
      historyItem.StateType <- "ApplicationState"
      historyItem.State <- WellCollectionDto.export wellCollection |> JsonConversions.export
      sendItem historyItem |> ignore
    | _, Error err ->
      Logger.error err
  
  let report wave wellCollection =
    EnvAccessor.env.VisionQuestDebuggingEnabled
    |> Option.fromBool
    |> Option.map (fun _ -> reportWellCollection wave wellCollection)
    |> ignore
    
  let quit () =
    EnvAccessor.env.VisionQuestDebuggingEnabled
    |> Option.fromBool
    |> Option.map (fun _ -> sendQuitSignal ())
    |> ignore
  
  if EnvAccessor.env.VisionQuestDebuggingEnabled  
  then VisionQuestTcp.listen ()