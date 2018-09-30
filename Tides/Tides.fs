namespace ChessPlus

module Tides =
  open Waves
  open Updaters
  open Finders
  open Fetchers
  open Result
  open JsonConversions
  open Moulds
  open Microsoft.FSharp.Reflection
  open Well
  
  type Tide<'W> = (string * string) * ((string * string) * Amplitude -> 'W -> 'W)
  
  let (<!>) = fun o f -> Option.map f o
  let (<!>>) = fun o f -> o |> Option.map f |> Option.flatten
  let upstream<'a> = export<'a> >> Udp.send >> ignore
  let upstreamTcp<'a> = export<'a> >> Tcp.send >> ignore
  
  let makeTide<'A, 'W> loc (action : 'A -> 'W -> 'W) : Tide<'W> =
    (loc, fun ((_ : string * string), (amplitude : Amplitude)) well ->
      let (_, v) = FSharpValue.GetUnionFields(amplitude, typeof<Amplitude>)
      action (v.[0] :?> 'A) well
    );
    
  let tide<'A> = makeTide<'A, LifeWell>
      
  let tides : Tide<LifeWell> list = [
    tide<UpdateTcpConnectionAmplitude> updateTcpConnectionLocation (fun amplitude well ->
      match well with
      | { Connection = { Tcp = false; Udp = false }} -> VisionQuest.quit ()
      | _ -> ()
      
      LifeWell.updateConnection (fun c -> { c with Tcp = amplitude.Connected }) well
    );
    
    tide<UpdateUdpConnectionAmplitude> updateUdpConnectionLocation (fun amplitude well ->
      match well with
      | { Connection = { Tcp = false; Udp = false }} -> VisionQuest.quit ()
      | _ -> ()
    
      LifeWell.updateConnection (fun c -> { c with Udp = amplitude.Connected }) well
    );
    
    tide<PlayerCreatedAmplitude> playerCreatedLocation (fun amplitude well ->
      let ({ Player = { Name = name }} : PlayerCreatedAmplitude) = amplitude
      let loginAmplitude : LoginAmplitude = { Name = name }
      LoginMould.export (loginLocation, loginAmplitude)
      |> upstreamTcp
      well
    );
    
    tide<ConfirmLoginAmplitude> confirmLoginLocation (fun amplitude well ->
      let ({ Player = player } : ConfirmLoginAmplitude) = amplitude
      LifeWell.updatePlayer (fun _ -> Some player) well
    );
    
    tide<ReportFailureAmplitude> reportFailureLocation (fun amplitude well ->
      let ({ Reason = reason } : ReportFailureAmplitude) = amplitude
      // TODO give proper feedback
      Logger.error reason
      
      well
    );
    
    tide<StartDuelAmplitude> startDuelLocation (fun amplitude well ->
      let ({ Duel = duel } : StartDuelAmplitude) = amplitude
      LifeWell.updateDuel (fun _ -> Some duel) well
    );
    
    tide<AddDuelistAmplitude> addDuelistLocation (fun amplitude well ->
      let ({ Duelist = duelist } : AddDuelistAmplitude) = amplitude
      LifeWell.updateDuelists (fun lst ->
        duelist::lst
        |> List.rev
      ) well
    );
    
    tide<UpdateDuelStateAmplitude> updateDuelStateLocation (fun amplitude well ->
      let ({ DuelState = duelState } : UpdateDuelStateAmplitude) = amplitude
      LifeWell.updateDuel (fun duel ->
        match duel with
        | Some duel ->
          Some ({ duel with DuelState = duelState } : Duel)
        | None ->
          None
      ) well
    );
    
  ]
  
  let buffTide<'A> = makeTide<'A, BuffWell>
  
  let buffTides : Tide<BuffWell> list = [
    buffTide<UpdateBuffsAmplitude> updateBuffsLocation (fun amplitude _ ->
      amplitude.Buffs
    );
  ]
  
  let uiTide<'A> = makeTide<'A, UiWell>
  
  let uiTides : Tide<UiWell> list = [
    uiTide<StartDuelAmplitude> startDuelLocation (fun amplitude well ->
      Pool.UI.closePopup PlayDuel well
    );   
    
    uiTide<DefaultAmplitude> requireLoginLocation (fun () well ->
      Pool.UI.openPopup Login well
    ); 
    
    uiTide<OpenPopupAmplitude> openPopupLocation (fun amplitude well ->
      Pool.UI.openPopup amplitude.Popup well
    );
    
    uiTide<ClosePopupAmplitude> closePopupLocation (fun amplitude well ->
      Pool.UI.closePopup amplitude.Popup well
    );
    
    uiTide<DefaultAmplitude> loginPopupClickOkLocation (fun amplitude well ->
      let name =
        findLoginPopupState well
        |> fun (s : LoginPopupState) -> s.Name
        
      let amplitude : LoginAmplitude = { Name = name }
      LoginMould.export (loginLocation, amplitude)
      |> upstream
      
      Pool.UI.closePopup Login well
    );
    
    uiTide<TextAmplitude> loginPopupNameChangeLocation (fun amplitude well ->
      Updaters.UI.updateLoginPopupState (fun s -> { s with Name = amplitude.Text }) well
    );
    
    uiTide<UiComponentAmplitude> playDuelPopupJoinButtonLocation (fun amplitude well ->
      Mould.export getOpenDuelsLocation
      |> upstream
      
      Pool.UI.UiComponent.set playDuelPopupJoinButtonLocation amplitude well
    );
    
    uiTide<UiComponentAmplitude> playDuelPopupNewButtonLocation (fun amplitude well ->
      Pool.UI.UiComponent.set playDuelPopupNewButtonLocation amplitude well
    );
    
    uiTide<AddOpenDuelsAmplitude> addOpenDuelsLocation (fun amplitude well ->
      List.length amplitude.Duels > 0
      |> Pool.UI.UiComponent.interactable playDuelPopupJoinButtonLocation <| well
    );
    
    uiTide<DefaultAmplitude> playDuelPopupClickNewLocation (fun amplitude well ->
      NewDuelMould.export (newDuelLocation, { Map = Classic })
      |> upstream
      
      Pool.UI.UiComponent.interactable playDuelPopupNewButtonLocation false
      >> Pool.UI.UiComponent.interactable playDuelPopupJoinButtonLocation false
      <| well
    );
    
    uiTide<DefaultAmplitude> playDuelPopupClickJoinLocation (fun amplitude well ->
      JoinDuelMould.export (joinDuelLocation, { ID = "any" })
      |> upstream
      
      Pool.UI.UiComponent.interactable playDuelPopupNewButtonLocation false
      >> Pool.UI.UiComponent.interactable playDuelPopupJoinButtonLocation false
      <| well    
    );
  ]
  
  let tileTide<'A> = makeTide<'A, TileWell>
  
  let tileTides : Tide<TileWell> list = [
    tileTide<StartDuelAmplitude> startDuelLocation (fun amplitude _ ->
      let ({ Tiles = tiles } : StartDuelAmplitude) = amplitude      
      tiles
    );  
  ]
  
  let tileSelectionTide<'A> = makeTide<'A, TileSelectionWell>
  
  let tileSelectionTides : Tide<TileSelectionWell> list = [
    tileSelectionTide<StartDuelAmplitude> startDuelLocation (fun amplitude _ ->
      let ({ TileSelections = selections } : StartDuelAmplitude) = amplitude      
      selections
    );  
    
    tileSelectionTide<ConquerTileAmplitude> conquerTileLocation (fun amplitude well ->
      let ({ Piece = piece; } : ConquerTileAmplitude) = amplitude
      let playerColor = (Types.Pieces.map (fun p -> p.Color) piece)
      
      Pool.TileSelections.deselect playerColor well
      |> Movements.resetMovableTiles playerColor
    );   
    
    tileSelectionTide<SelectClientTileAmplitude> selectClientTileLocation (fun amplitude well ->
      let (<*>) = Option.apply
      
      if Movements.isMovableTile amplitude.Coordinate well then
        Some (fun from piece -> 
          MovePieceMould.export (movePieceLocation, { From = from; To = amplitude.Coordinate; Piece = piece })
          |> upstream)
        <*> fetchOwnSelectedTileCoords ()
        <*> fetchOwnSelectedPiece ()
        |> ignore
        
        well
      else
        SelectClientTileMould.export (selectClientTileLocation, amplitude)
        |> upstream
        
        well
    );   
    
    tileSelectionTide<SelectTileAmplitude> selectTileLocation (fun amplitude well ->
      let ({ Player = playerColor; Coordinate = coord } : SelectTileAmplitude) = amplitude
   
      Pool.TileSelections.deselect playerColor well
      |> Pool.TileSelections.select coord playerColor
      |> Movements.updateMovableTiles playerColor
      |> Result.bind (Conquers.updateConquerableTiles playerColor false)
      |> Result.expect well
    );
    
    tileSelectionTide<DefaultAmplitude> deselectTileLocation (fun amplitude well ->
      DeselectTileMould.export (deselectTileLocation, amplitude)
      |> upstream
      
      well
    );    
    
    tileSelectionTide<ConfirmDeselectTileAmplitude> confirmDeselectTileLocation (fun amplitude well ->
      let ({ Player = playerColor } : ConfirmDeselectTileAmplitude) = amplitude
      
      Movements.resetMovableTiles playerColor well
      |> Pool.TileSelections.deselect playerColor
    );
    
    tileSelectionTide<MovePieceAmplitude> movePieceLocation (fun amplitude well ->
      let ({ Piece = piece } : MovePieceAmplitude) = amplitude
      let color = Types.Pieces.map (fun p -> p.Color) piece
      
      Movements.resetMovableTiles color well
      |> Pool.TileSelections.deselect color
    );       
  ]
  
  let pieceTide<'A> = makeTide<'A, PieceWell>
  
  let pieceTides : Tide<PieceWell> list = [
    pieceTide<StartDuelAmplitude> startDuelLocation (fun amplitude _ ->
      let ({ Pieces = pieces } : StartDuelAmplitude) = amplitude      
      pieces
    );  
          
    pieceTide<AddPieceAmplitude> addPieceLocation (fun amplitude well ->
      let ({ Piece = piece; Coordinate = coord } : AddPieceAmplitude) = amplitude
      Pieces.updatePiece coord (fun _ -> Some piece) well
    );
    
    pieceTide<RemovePieceAmplitude> removePieceLocation (fun amplitude well ->
      let ({ Coordinate = coord } : RemovePieceAmplitude) = amplitude
      Pieces.updatePiece coord (fun _ -> None) well
    );
    
    pieceTide<ConquerTileAmplitude> conquerTileLocation (fun amplitude well ->
      let ({ Piece = piece; To = t } : ConquerTileAmplitude) = amplitude
      
      Pool.Pieces.movePiece piece t well
    );
    
    pieceTide<PromotePieceAmplitude> promotePieceLocation (fun amplitude well ->
      let ({ Piece = piece } : PromotePieceAmplitude) = amplitude
      
      Updaters.Pieces.updatePieceWhere
        (fun _ p -> Types.Pieces.id p = Types.Pieces.id piece)
        (fun coord _ -> Types.Pieces.update (fun p -> { p with Coordinate = Some coord }) piece)
        well
    );
 
  ]
  
  let ruleTide<'A> = makeTide<'A, RuleWell>
  
  let ruleTides : Tide<RuleWell> list = [
    ruleTide<StartDuelAmplitude> startDuelLocation (fun amplitude _ ->
      let ({ Rules = rules } : StartDuelAmplitude) = amplitude      
      rules
    );  
  ]