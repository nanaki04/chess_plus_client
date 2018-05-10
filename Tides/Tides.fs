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
    
    // TODO request player info from player
    tide<DefaultAmplitude> requireLoginLocation (fun () well ->
      let amplitude : LoginAmplitude = { Name = "Sheep" }
      LoginMould.export (loginLocation, amplitude)
      |> upstream
      well
    );
    
    tide<PlayerCreatedAmplitude> playerCreatedLocation (fun amplitude well ->
      let ({ Player = { Name = name }} : PlayerCreatedAmplitude) = amplitude
      let loginAmplitude : LoginAmplitude = { Name = name }
      LoginMould.export (loginLocation, loginAmplitude)
      |> upstreamTcp
      well
    );
    
    // TODO send to server
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
      
      // TODO TEMP
      JoinDuelMould.export (joinDuelLocation, { ID = "192.168.13.235:5000" })
      |> upstream
      
      well
    );
    
    tide<NewDuelAmplitude> newDuelLocation (fun amplitude well ->
      NewDuelMould.export (newDuelLocation, amplitude)
      |> upstream
      well
    );
    
    tide<StartDuelAmplitude> startDuelLocation (fun amplitude well ->
      let ({ Duel = duel } : StartDuelAmplitude) = amplitude
      updateDuel (fun _ -> Some duel) well
    );
    
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
      updatePiece from (fun _ -> None) well
      |> updatePiece t (fun _ -> Some piece)
    );
    
    tide<SelectClientTileAmplitude> selectClientTileLocation (fun amplitude well ->
      SelectClientTileMould.export (selectClientTileLocation, amplitude)
      |> upstream
      well
//      let ({ Coordinate = coord } : SelectClientTileAmplitude) = amplitude
//      well
//      |> Pool.findClientDuelist
//      <!> (fun d ->
//        SelectTileMould.export (selectTileLocation, { Player = d.Color; Coordinate = coord })
//        |> upstream    
//        
//        Pool.deselect d.Color well
//        |> updateTile coord (fun t -> { t with SelectedBy = Some d.Color })
//      )
//      |> Option.defaultValue well
    );   
     
    tide<SelectTileAmplitude> selectTileLocation (fun amplitude well ->
      let ({ Player = playerColor; Coordinate = coord } : SelectTileAmplitude) = amplitude
      Pool.deselect playerColor well
      |> updateTile coord (fun t -> { t with SelectedBy = Some playerColor })
    );
    
    tide<DefaultAmplitude> deselectTileLocation (fun amplitude well ->
      DeselectTileMould.export (deselectTileLocation, amplitude)
      |> upstream    
      well
    );    
    
    tide<ConfirmDeselectTileAmplitude> confirmDeselectTileLocation (fun amplitude well ->
      let ({ Player = playerColor } : ConfirmDeselectTileAmplitude) = amplitude
      Pool.deselect playerColor well
    );  
  ]