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
  open Types
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
      match well, amplitude.Connected with
      | { Connection = { Udp = false }}, false -> VisionQuest.quit ()
      | _ -> ()
      
      LifeWell.updateConnection (fun c -> { c with Tcp = amplitude.Connected }) well
    );
    
    tide<UpdateUdpConnectionAmplitude> updateUdpConnectionLocation (fun amplitude well ->
      match well, amplitude.Connected with
      | { Connection = { Tcp = false; }}, false -> VisionQuest.quit ()
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
    
    tide<RemoveDuelistAmplitude> removeDuelistLocation (fun amplitude well ->
      LifeWell.updateDuelists (fun lst ->
        List.filter (fun duelist ->
          duelist.Name <> amplitude.Duelist.Name
        ) lst
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
    
    uiTide<UiComponentAmplitude> duelStatePlateClickJoinLocation (fun amplitude well ->
      Mould.export getOpenDuelsLocation
      |> upstream
      
      Pool.UI.UiComponent.set duelStatePlateClickJoinLocation amplitude well
    );
    
    uiTide<UiComponentAmplitude> playDuelPopupNewButtonLocation (fun amplitude well ->
      Pool.UI.UiComponent.set playDuelPopupNewButtonLocation amplitude well
    );
    
    uiTide<AddOpenDuelsAmplitude> addOpenDuelsLocation (fun amplitude well ->
      let hasOpenDuels = List.length amplitude.Duels > 0
      
      Pool.UI.UiComponent.interactable playDuelPopupJoinButtonLocation hasOpenDuels well
      |> Pool.UI.UiComponent.interactable playDuelPopupNewButtonLocation true
      |> Pool.UI.UiComponent.interactable duelStatePlateJoinButtonLocation hasOpenDuels
      |> Pool.UI.UiComponent.interactable duelStatePlateNewButtonLocation true
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
    
    uiTide<UiComponentAmplitude> whitePlayerInformationPlateLocation (fun amplitude well ->
      Pool.UI.UiComponent.set whitePlayerInformationPlateLocation amplitude well
    );
    
    uiTide<UiComponentAmplitude> blackPlayerInformationPlateLocation (fun amplitude well ->
      Pool.UI.UiComponent.set blackPlayerInformationPlateLocation amplitude well
    );
    
    uiTide<StartDuelAmplitude> startDuelLocation (fun amplitude well ->
      let ({ Duel = { Duelists = duelists }} : StartDuelAmplitude) = amplitude
      
      let well =
        Pool.UI.closePopup PlayDuel well
        |> Pool.UI.closePopup AwaitRematchResponse
        |> Pool.UI.UiComponent.visible gameMenuLocation true
        |> Pool.UI.UiComponent.interactable gameMenuForfeitButtonLocation true
        |> Pool.UI.UiComponent.interactable gameMenuRemiseButtonLocation true
        |> Pool.UI.UiComponent.interactable duelStatePlateNewButtonLocation true
        |> Pool.UI.UiComponent.interactable duelStatePlateRematchButtonLocation true
        |> Pool.UI.UiComponent.interactable duelStatePlateJoinButtonLocation true
        |> Pool.UI.UiComponent.visible whitePlayerInformationPlateLocation false
        |> Pool.UI.UiComponent.visible blackPlayerInformationPlateLocation false
        
      List.fold (fun well duelist ->
        match duelist with
        | { Color = White; Name = name } ->
          Pool.UI.UiComponent.visible whitePlayerInformationPlateLocation true well
          |> Pool.UI.DynamicText.set whitePlayerNameDynamicText name
        | { Color = Black; Name = name } ->
          Pool.UI.UiComponent.visible blackPlayerInformationPlateLocation true well
          |> Pool.UI.DynamicText.set blackPlayerNameDynamicText name
      ) well duelists
    );
    
    uiTide<UiComponentAmplitude> gameMenuLocation (fun amplitude well ->
      Pool.UI.UiComponent.set gameMenuLocation amplitude well
    );  
      
    uiTide<DefaultAmplitude> gameMenuClickForfeitButtonLocation (fun _ well ->
      DefaultMould.export (forfeitDuelLocation, ())
      |> upstream
      
      well
    );
    
    uiTide<DefaultAmplitude> gameMenuClickRemiseButtonLocation (fun _ well ->
      DefaultMould.export (proposeRemiseLocation, ())
      |> upstream
      
      Pool.UI.openPopup AwaitRemiseResponse well
      |> Pool.UI.UiComponent.interactable gameMenuForfeitButtonLocation false
      |> Pool.UI.UiComponent.interactable gameMenuRemiseButtonLocation false
    );
    
    uiTide<DefaultAmplitude> proposeRemiseLocation (fun _ well ->
      Pool.UI.openPopup ConfirmRemise well
      |> Pool.UI.UiComponent.interactable gameMenuForfeitButtonLocation false
      |> Pool.UI.UiComponent.interactable gameMenuRemiseButtonLocation false
    );
    
    uiTide<DefaultAmplitude> confirmRemisePopupClickYesButtonLocation (fun _ well ->
      DefaultMould.export (remiseLocation, ())
      |> upstreamTcp
      
      Pool.UI.closePopup ConfirmRemise well
    );
    
    uiTide<DefaultAmplitude> confirmRemisePopupClickNoButtonLocation (fun _ well ->
      DefaultMould.export (refuseRemiseLocation, ())
      |> upstreamTcp
      
      Pool.UI.closePopup ConfirmRemise well
      |> Pool.UI.UiComponent.interactable gameMenuForfeitButtonLocation true
      |> Pool.UI.UiComponent.interactable gameMenuRemiseButtonLocation true
    );
    
    uiTide<DefaultAmplitude> refuseRemiseLocation (fun _ well ->
      Pool.UI.closePopup AwaitRemiseResponse well
      |> Pool.UI.openPopup RemiseRefused
      |> Pool.UI.UiComponent.interactable gameMenuForfeitButtonLocation true
      |> Pool.UI.UiComponent.interactable gameMenuRemiseButtonLocation true
    );
    
    uiTide<DefaultAmplitude> remiseRefusedPopupClickOkButtonLocation (fun _ well ->
      Pool.UI.closePopup RemiseRefused well
    );
    
    uiTide<UiComponentAmplitude> duelStatePlateMenuLocation (fun amplitude well ->
      Pool.UI.UiComponent.set duelStatePlateMenuLocation amplitude well
    );    
    
    uiTide<UpdateDuelStateAmplitude> updateDuelStateLocation (fun amplitude well ->
      let setGameMenuEnabled enabled well =
        Pool.UI.UiComponent.interactable gameMenuForfeitButtonLocation enabled well
        |> Pool.UI.UiComponent.interactable gameMenuRemiseButtonLocation enabled
        |> Pool.UI.closePopup AwaitRemiseResponse
        |> Pool.UI.closePopup ConfirmRemise
    
      match amplitude.DuelState with
      | Ended (Win _) ->
        Pool.UI.UiComponent.visible duelStatePlateMenuLocation true well
        |> setGameMenuEnabled false
      | Ended Remise ->
        Pool.UI.UiComponent.visible duelStatePlateMenuLocation true well
        |> setGameMenuEnabled false
      | Ended (RequestRematch color) ->
        let well =
          Pool.UI.UiComponent.visible duelStatePlateMenuLocation false well
          |> setGameMenuEnabled false
        
        if Pool.isPlayer color
        then Pool.UI.openPopup AwaitRematchResponse well
        else Pool.UI.openPopup ConfirmRematch well
      | _ ->
        Pool.UI.UiComponent.visible duelStatePlateMenuLocation false well
        |> setGameMenuEnabled true
    );
    
    uiTide<AddDuelistAmplitude> addDuelistLocation (fun amplitude well ->
      match amplitude with
      | { Duelist = { Color = White; Name = name }} ->
        Pool.UI.UiComponent.visible whitePlayerInformationPlateLocation true well
        |> Pool.UI.DynamicText.set whitePlayerNameDynamicText name
      | { Duelist = { Color = Black; Name = name }} ->
        Pool.UI.UiComponent.visible blackPlayerInformationPlateLocation true well
        |> Pool.UI.DynamicText.set blackPlayerNameDynamicText name
      |> Pool.UI.UiComponent.interactable duelStatePlateRematchButtonLocation true
    );
    
    uiTide<RemoveDuelistAmplitude> removeDuelistLocation (fun amplitude well ->
      match amplitude with
      | { Duelist = { Color = White; }} ->
        Pool.UI.UiComponent.visible whitePlayerInformationPlateLocation false well
        |> Pool.UI.DynamicText.set whitePlayerNameDynamicText ""
      | { Duelist = { Color = Black; }} ->
        Pool.UI.UiComponent.visible blackPlayerInformationPlateLocation false well
        |> Pool.UI.DynamicText.set blackPlayerNameDynamicText ""
      // TODO count duelist when more than 2 players would be supported
      |> Pool.UI.UiComponent.interactable duelStatePlateRematchButtonLocation false
    );
    
    uiTide<UiComponentAmplitude> duelStatePlateRematchButtonLocation (fun amplitude well ->
      Pool.UI.UiComponent.set duelStatePlateRematchButtonLocation amplitude well
    );
    
    uiTide<UiComponentAmplitude> duelStatePlateNewButtonLocation (fun amplitude well ->
      Pool.UI.UiComponent.set duelStatePlateNewButtonLocation amplitude well
    );
    
    uiTide<UiComponentAmplitude> duelStatePlateJoinButtonLocation (fun amplitude well ->
      Pool.UI.UiComponent.set duelStatePlateJoinButtonLocation amplitude well
    );  
          
    uiTide<UiComponentAmplitude> gameMenuForfeitButtonLocation (fun amplitude well ->
      Pool.UI.UiComponent.set gameMenuForfeitButtonLocation amplitude well
    );
    
    uiTide<UiComponentAmplitude> gameMenuRemiseButtonLocation (fun amplitude well ->
      Pool.UI.UiComponent.set gameMenuRemiseButtonLocation amplitude well
    );   
    
    uiTide<DefaultAmplitude> duelStatePlateClickRematchLocation (fun _ well ->
      DefaultMould.export (requestRematchLocation, ())
      |> upstreamTcp
      
      Pool.UI.UiComponent.interactable duelStatePlateRematchButtonLocation false well
      |> Pool.UI.UiComponent.interactable duelStatePlateNewButtonLocation false
      |> Pool.UI.UiComponent.interactable duelStatePlateJoinButtonLocation false
    );
    
    uiTide<DefaultAmplitude> duelStatePlateClickNewLocation (fun _ well ->
      DefaultMould.export (removeDuelistLocation, ())
      |> upstreamTcp
      
      NewDuelMould.export (newDuelLocation, { Map = Classic })
      |> upstreamTcp
      
      Pool.UI.UiComponent.interactable duelStatePlateRematchButtonLocation false well
      |> Pool.UI.UiComponent.interactable duelStatePlateNewButtonLocation false
      |> Pool.UI.UiComponent.interactable duelStatePlateJoinButtonLocation false
    );
    
    uiTide<DefaultAmplitude> duelStatePlateClickJoinLocation (fun _ well ->
      DefaultMould.export (removeDuelistLocation, ())
      |> upstreamTcp
    
      JoinDuelMould.export (joinDuelLocation, { ID = "any" })
      |> upstreamTcp
    
      Pool.UI.UiComponent.interactable duelStatePlateRematchButtonLocation false well
      |> Pool.UI.UiComponent.interactable duelStatePlateNewButtonLocation false
      |> Pool.UI.UiComponent.interactable duelStatePlateJoinButtonLocation false
    );
    
    uiTide<DefaultAmplitude> requestRematchLocation (fun _ well ->
      Pool.UI.openPopup ConfirmRematch well
      |> Pool.UI.UiComponent.interactable duelStatePlateRematchButtonLocation false
      |> Pool.UI.UiComponent.interactable duelStatePlateNewButtonLocation false
      |> Pool.UI.UiComponent.interactable duelStatePlateJoinButtonLocation false      
    );
    
    uiTide<DefaultAmplitude> confirmRematchPopupClickYesLocation (fun _ well ->
      DefaultMould.export (rematchLocation, ())
      |> upstreamTcp
    
      Pool.UI.closePopup ConfirmRematch well
    );
    
    uiTide<DefaultAmplitude> confirmRematchPopupClickNoLocation (fun _ well ->
      DefaultMould.export (refuseRematchLocation, ())
      |> upstreamTcp
      
      Pool.UI.closePopup ConfirmRematch well
      |> Pool.UI.UiComponent.interactable duelStatePlateRematchButtonLocation true
      |> Pool.UI.UiComponent.interactable duelStatePlateNewButtonLocation true
      |> Pool.UI.UiComponent.interactable duelStatePlateJoinButtonLocation true
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