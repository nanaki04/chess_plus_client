﻿namespace ChessPlus

module Tides =
  open Waves
  open Updaters
  open Finders
  open Result
  open Microsoft.FSharp.Reflection
  
  let (<!>) = fun o f -> Option.map f o
  let (<!>>) = fun o f -> o |> Option.map f |> Option.flatten
  
  let tide<'A> loc (action : 'A -> LifeWell -> LifeWell) =
    (loc, fun ((_ : string * string), (amplitude : Amplitude)) well ->
      let (_, v) = FSharpValue.GetUnionFields(amplitude, typeof<Amplitude>)
      action (v.[0] :?> 'A) well
    );
      
  let tides = [
    tide<AddTileAmplitude> addTileLocation (fun amplitude well ->
      let { Coordinate = coordinate; Tile = tile } = amplitude
      updateTile coordinate (fun _ -> tile) well
    );
    
    // TODO send to server
    tide<LoginAmplitude> loginLocation (fun amplitude well ->
      let ({ Name = name } : LoginAmplitude) = amplitude
      updatePlayer (fun _ -> { Name = name } : Player) well
    );
    
    tide<ConfirmLoginAmplitude> confirmLoginLocation (fun amplitude well ->
      let ({ Name = name } : ConfirmLoginAmplitude) = amplitude
      updatePlayer (fun _ -> { Name = name } : Player) well
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
      well
    );
    
    tide<StartDuelAmplitude> startDuelLocation (fun amplitude well ->
      let ({ Duel = duel } : StartDuelAmplitude) = amplitude
      updateDuel (fun _ -> Some duel) well
    );
    
    // TODO send to server
    tide<JoinDuelAmplitude> joinDuelLocation (fun amplitude well ->
      let ({ Player = player; ID = id } : JoinDuelAmplitude) = amplitude
      updateDuelists (fun lst ->
        let color = if lst.Length % 2 = 0 then White else Black
        { Name = player.Name; Color = color }::lst
        |> List.rev
      ) well
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
    
    tide<SelectTileAmplitude> selectTileLocation (fun amplitude well ->
      let ({ Player = playerColor; Coordinate = coord } : SelectTileAmplitude) = amplitude
      updateTile coord (fun t -> { t with SelectedBy = Some playerColor }) well
    );
    
    tide<DeselectTileAmplitude> deselectTileLocation (fun amplitude well ->
      let ({ Player = playerColor } : DeselectTileAmplitude) = amplitude
      let colWithColor (acc : Option<Column>) (col : Column) (tile : Tile) : Option<Column> =
        Option.orElseIf (tile.SelectedBy = (Some playerColor)) (Some col) acc
      let coordWithColor (acc : Option<Row * Column>) row cols : Option<Coordinate> =
        Option.orElse ((Map.fold colWithColor None cols) <!> fun col -> (row, col)) None

      well
      |> findTiles
      |> fun x -> x
      <!>> Map.fold coordWithColor None
      <!> fun coord -> updateTile coord (fun t -> { t with SelectedBy = None }) well
      |> Option.defaultValue well
    );  
    
//    (addTileLocation, fun (_, amplitude) well ->
//      match amplitude with
//      | AddTileAmplitude { Coordinate = coordinate; Tile = tile } ->
//        updateTile coordinate (fun _ -> tile) well
//      | _ -> well
//    );
//    
//    (startDuelLocation, fun (_, amplitude) well ->
//      match amplitude with
//      | StartDuelAmplitude { Duel = duel } ->
//        updateDuel (fun _ -> duel) well
//      | _ -> well
//    );
  ]