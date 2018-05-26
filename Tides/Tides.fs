namespace ChessPlus

module Tides =
  open Waves
  open Updaters
  open Finders
  open Result
  open JsonConversions
  open Moulds
  open Microsoft.FSharp.Reflection
  
  let (<!>) = fun o f -> Option.map f o
  let (<!>>) = fun o f -> o |> Option.map f |> Option.flatten
  let upstream<'a> = export<'a> >> Udp.send >> ignore
  let upstreamTcp<'a> = export<'a> >> Tcp.send >> ignore
  
  let tide<'A> loc (action : 'A -> LifeWell -> LifeWell) =
    (loc, fun ((_ : string * string), (amplitude : Amplitude)) well ->
      let (_, v) = FSharpValue.GetUnionFields(amplitude, typeof<Amplitude>)
      action (v.[0] :?> 'A) well
    );
      
  let tides = [
    tide<UpdateTcpConnectionAmplitude> updateTcpConnectionLocation (fun amplitude well ->
      updateConnection (fun c -> { c with Tcp = amplitude.Connected }) well
    );
    
    tide<UpdateUdpConnectionAmplitude> updateUdpConnectionLocation (fun amplitude well ->
      updateConnection (fun c -> { c with Udp = amplitude.Connected }) well
    );
  
    tide<AddTileAmplitude> addTileLocation (fun amplitude well ->
      let { Coordinate = coordinate; Tile = tile } = amplitude
      updateTile coordinate (fun _ -> tile) well
    );
    
    tide<DefaultAmplitude> requireLoginLocation (fun () well ->
      Pool.openPopup Login well
    );
    
    tide<PlayerCreatedAmplitude> playerCreatedLocation (fun amplitude well ->
      let ({ Player = { Name = name }} : PlayerCreatedAmplitude) = amplitude
      let loginAmplitude : LoginAmplitude = { Name = name }
      LoginMould.export (loginLocation, loginAmplitude)
      |> upstreamTcp
      well
    );
    
    // @depricated
    tide<LoginAmplitude> loginLocation (fun amplitude well ->
      LoginMould.export (loginLocation, amplitude)
      |> upstream
      well
    );
    
    tide<ConfirmLoginAmplitude> confirmLoginLocation (fun amplitude well ->
      let ({ Player = player } : ConfirmLoginAmplitude) = amplitude
      updatePlayer (fun _ -> Some player) well
    );
    
    // @depricated
    tide<FailedLoginAmplitude> failedLoginLocation (fun amplitude well ->
      let ({ Reason = reason } : FailedLoginAmplitude) = amplitude
      // TODO give proper feedback
      Logger.error reason
      well
    );
    
    tide<ReportFailureAmplitude> reportFailureLocation (fun amplitude well ->
      let ({ Reason = reason } : ReportFailureAmplitude) = amplitude
      // TODO give proper feedback
      Logger.error reason
      
      well
    );
    
    // @depricated
    tide<NewDuelAmplitude> newDuelLocation (fun amplitude well ->
      NewDuelMould.export (newDuelLocation, amplitude)
      |> upstream
      well
    );
    
    tide<StartDuelAmplitude> startDuelLocation (fun amplitude well ->
      let ({ Duel = duel } : StartDuelAmplitude) = amplitude
      updateDuel (fun _ -> Some duel)
      >> Pool.closePopup PlayDuel
      <| well
    );
    
    // @depricated
    tide<JoinDuelAmplitude> joinDuelLocation (fun amplitude well ->
      JoinDuelMould.export (joinDuelLocation, amplitude)
      |> upstream
      well
    );
    
    tide<AddDuelistAmplitude> addDuelistLocation (fun amplitude well ->
      let ({ Duelist = duelist } : AddDuelistAmplitude) = amplitude
      updateDuelists (fun lst ->
        duelist::lst
        |> List.rev
      ) well
    );
    
    tide<SetupBoardAmplitude> setupBoardLocation (fun amplitude well ->
      let ({ Board = board } : SetupBoardAmplitude) = amplitude
      updateBoard (fun _ -> board) well
    );
    
    tide<AddPieceAmplitude> addPieceLocation (fun amplitude well ->
      let ({ Piece = piece; Coordinate = coord } : AddPieceAmplitude) = amplitude
      updatePiece coord (fun _ -> Some piece) well
    );
    
    tide<RemovePieceAmplitude> removePieceLocation (fun amplitude well ->
      let ({ Coordinate = coord } : RemovePieceAmplitude) = amplitude
      updatePiece coord (fun _ -> None) well
    );
    
    // TODO send to server
    tide<MovePieceAmplitude> movePieceLocation (fun amplitude well ->
      let ({ Piece = piece; From = from; To = t } : MovePieceAmplitude) = amplitude
      updatePiece from (fun _ -> None) well
      |> updatePiece t (fun _ -> Some piece)
    );
    
    tide<ConquerTileAmplitude> conquerTileLocation (fun amplitude well ->
      let ({ Piece = piece; From = from; To = t } : ConquerTileAmplitude) = amplitude
      let playerColor = (Types.Pieces.map (fun p -> p.Color) piece)
      
      updatePiece from (fun _ -> None) well
      |> updatePiece t (fun _ -> Some piece)
      |> Movements.resetMovableTiles playerColor
      |> Pool.deselect playerColor
    );
    
    tide<SelectClientTileAmplitude> selectClientTileLocation (fun amplitude well ->
      let (<*>) = Option.apply
      
      if Movements.isMovableTile amplitude.Coordinate well then
        Some (fun from piece -> 
          MovePieceMould.export (movePieceLocation, { From = from; To = amplitude.Coordinate; Piece = piece })
          |> upstream)
        <*> findOwnSelectedTileCoords well
        <*> findOwnSelectedPiece well
        |> ignore
        
        well
      else
        SelectClientTileMould.export (selectClientTileLocation, amplitude)
        |> upstream
        
        well
    );   
    
    tide<SelectTileAmplitude> selectTileLocation (fun amplitude well ->
      let ({ Player = playerColor; Coordinate = coord } : SelectTileAmplitude) = amplitude
   
      Logger.now "selectTile:deselect"
      Pool.deselect playerColor well
      |> Logger.nowP "SelectTile:select"
      |> Pool.select coord playerColor
      |> Logger.nowP "SelectTile:updateMovableTiles"
      |> Movements.updateMovableTiles playerColor
      |> Logger.nowP "SelectTile:done"
      // TODO |> Conquers.updateConquerableTiles playerColor
    );
    
    tide<DefaultAmplitude> deselectTileLocation (fun amplitude well ->
      DeselectTileMould.export (deselectTileLocation, amplitude)
      |> upstream
      
      well
    );    
    
    tide<ConfirmDeselectTileAmplitude> confirmDeselectTileLocation (fun amplitude well ->
      let ({ Player = playerColor } : ConfirmDeselectTileAmplitude) = amplitude
      Movements.resetMovableTiles playerColor well
      |> Pool.deselect playerColor
    );
    
    tide<OpenPopupAmplitude> openPopupLocation (fun amplitude well ->
      Pool.openPopup amplitude.Popup well
    );
    
    tide<ClosePopupAmplitude> closePopupLocation (fun amplitude well ->
      Pool.closePopup amplitude.Popup well
    );
    
    tide<DefaultAmplitude> loginPopupClickOkLocation (fun amplitude well ->
      let name =
        findLoginPopupState well
        |> fun (s : LoginPopupState) -> s.Name
        
      let amplitude : LoginAmplitude = { Name = name }
      LoginMould.export (loginLocation, amplitude)
      |> upstream
      
      Pool.closePopup Login well
    );
    
    tide<TextAmplitude> loginPopupNameChangeLocation (fun amplitude well ->
      updateLoginPopupState (fun s -> { s with Name = amplitude.Text }) well
    );
    
    tide<UiComponentAmplitude> playDuelPopupJoinButtonLocation (fun amplitude well ->
      Mould.export getOpenDuelsLocation
      |> upstream
      
      Pool.UiComponent.set playDuelPopupJoinButtonLocation amplitude well
    );
    
    tide<UiComponentAmplitude> playDuelPopupNewButtonLocation (fun amplitude well ->
      Pool.UiComponent.set playDuelPopupNewButtonLocation amplitude well
    );
    
    tide<AddOpenDuelsAmplitude> addOpenDuelsLocation (fun amplitude well ->
      List.length amplitude.Duels > 0
      |> Pool.UiComponent.interactable playDuelPopupJoinButtonLocation <| well
    );
    
    tide<DefaultAmplitude> playDuelPopupClickNewLocation (fun amplitude well ->
      NewDuelMould.export (newDuelLocation, { Map = Classic })
      |> upstream
      
      Pool.UiComponent.interactable playDuelPopupNewButtonLocation false
      >> Pool.UiComponent.interactable playDuelPopupJoinButtonLocation false
      <| well
    );
    
    tide<DefaultAmplitude> playDuelPopupClickJoinLocation (fun amplitude well ->
      JoinDuelMould.export (joinDuelLocation, { ID = "any" })
      |> upstream
      
      Pool.UiComponent.interactable playDuelPopupNewButtonLocation false
      >> Pool.UiComponent.interactable playDuelPopupJoinButtonLocation false
      <| well    
    );
  ]