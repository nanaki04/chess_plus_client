namespace ChessPlus

module Waves =
  open Well
  
  type DefaultAmplitude = unit
  type TextAmplitude = { Text : string }
  type UiComponentAmplitude = UiComponent
  
  type UpdateTcpConnectionAmplitude = { Connected : bool }
  let updateTcpConnectionLocation = ("connection", "tcp")
  let updateTcpConnectionWave amplitude = (updateTcpConnectionLocation, amplitude)
  
  type UpdateUdpConnectionAmplitude = { Connected : bool }
  let updateUdpConnectionLocation = ("connection", "udp")
  let updateUdpConnectionWave amplitude = (updateUdpConnectionLocation, amplitude)
  
  type AddTileAmplitude = { Coordinate : Coordinate; Tile : Tile }
  let addTileLocation = ("tile", "add")
  let addTileWave amplitude = (addTileLocation, amplitude)
  
  let requireLoginLocation = ("player", "in")
  let requireLoginWave amplitude = (requireLoginLocation, amplitude)
  
  type LoginAmplitude = { Name : Name }
  let loginLocation = ("player", "join")
  let loginWave amplitude = (loginLocation, amplitude)
  
  type PlayerCreatedAmplitude = { Player : Player }
  let playerCreatedLocation = ("player", "created")
  let playerCreatedWave amplitude = (playerCreatedLocation, amplitude)
  
  type ConfirmLoginAmplitude = { Player : Player }
  let confirmLoginLocation = ("player", "add")
  let confirmLoginWave amplitude = (confirmLoginLocation, amplitude)
  
  type FailedLoginAmplitude = { Reason : string }
  let failedLoginLocation = ("player", "error")
  let failedLoginWave amplitude = (failedLoginLocation, amplitude)
  
  type ReportFailureAmplitude = { Reason : string }
  let reportFailureLocation = ("global", "error")
  let reportFailureWave amplitude = (reportFailureLocation, amplitude)
  
  type NewDuelAmplitude = { Map : Territory }
  let newDuelLocation = ("duel", "new")
  let newDuelWave amplitude = (newDuelLocation, amplitude)
  
  type StartDuelAmplitude = {
    Duel : Duel;
    Tiles : TileWell;
    TileSelections : TileSelectionWell;
    Pieces : PieceWell;
    Rules : RuleWell;
    Buffs : BuffWell;
  }
  let startDuelLocation = ("duel", "add")
  let startDuelWave amplitude = (startDuelLocation, amplitude)
  
  let rematchLocation = ("duel", "rematch")
  
  type JoinDuelAmplitude = { ID : string }
  let joinDuelLocation = ("duelist", "join")
  let joinDuelWave amplitude = (joinDuelLocation, amplitude)
  
  type AddDuelistAmplitude = { Duelist : Duelist }
  let addDuelistLocation = ("duelist", "add")
  let addDuelistWave amplitude = (addDuelistLocation, amplitude)
  
  type RemoveDuelistAmplitude = { Duelist : Duelist }
  let removeDuelistLocation = ("duelist", "remove")
  let removeDuelistWave amplitude = (removeDuelistLocation, amplitude)
  
  let forfeitDuelLocation = ("duelist", "forfeit") 
  let proposeRemiseLocation = ("duelist", "propose_remise")
  let remiseLocation = ("duelist", "remise") 
  let refuseRemiseLocation = ("duelist", "refuse_remise")
  let requestRematchLocation = ("duelist", "request_rematch")
  let refuseRematchLocation = ("duelist", "refuse_rematch")
  
  type AddPieceAmplitude = { Piece : Pieces; Coordinate : Coordinate }
  let addPieceLocation = ("piece", "add")
  let addPieceWave amplitude = (addPieceLocation, amplitude)
  
  type RemovePieceAmplitude = { Coordinate : Coordinate }
  let removePieceLocation = ("piece", "remove")
  let removePieceWave amplitude = (removePieceLocation, amplitude)
  
  type MovePieceAmplitude = { Piece : Pieces; From : Coordinate; To : Coordinate }
  let movePieceLocation = ("piece", "move")
  let movePieceWave amplitude = (movePieceLocation, amplitude)
  
  type ConquerTileAmplitude = { Piece : Pieces; From : Coordinate; To : Coordinate }
  let conquerTileLocation = ("piece", "conquer")
  let conquerTileWave amplitude = (conquerTileLocation, amplitude)
  
  type PromotePieceAmplitude = { Piece : Pieces }
  let promotePieceLocation = ("piece", "promote")
  let promotePieceWave amplitude = (promotePieceLocation, amplitude)

  type UpdateBuffsAmplitude = { Buffs : BuffWell }
  let updateBuffsLocation = ("buffs", "update")
  let updateBuffsWave amplitude = (updateBuffsLocation, amplitude)
  
  type SelectTileAmplitude = { Player : Color; Coordinate : Coordinate }
  let selectTileLocation = ("tile", "confirm_select")
  let selectTileWave amplitude = (selectTileLocation, amplitude)
  
  type SelectClientTileAmplitude = { Coordinate : Coordinate }
  let selectClientTileLocation = ("tile", "select")
  let selectClientTileWave amplitude = (selectClientTileLocation, amplitude)

  let deselectTileLocation = ("tile", "deselect")
  let deselectTileWave amplitude = (deselectTileLocation, amplitude)
  
  type ConfirmDeselectTileAmplitude = { Player : Color }
  let confirmDeselectTileLocation = ("tile", "confirm_deselect")
  let confirmDeselectTileWave amplitude = (confirmDeselectTileLocation, amplitude)
  
  type UpdateDuelStateAmplitude = { DuelState : DuelState }
  let updateDuelStateLocation = ("duel_state", "update")
  let updateDuelStateWave amplitude = (updateDuelStateLocation, amplitude)

  type OpenPopupAmplitude = { Popup : Popup }
  let openPopupLocation = ("popup", "open")
  let openPopupWave amplitude = (openPopupLocation, amplitude)
  
  type ClosePopupAmplitude = { Popup : Popup }
  let closePopupLocation = ("popup", "close")
  let closePopupWave amplitude = (closePopupLocation, amplitude)
  
  let getOpenDuelsLocation = ("open_duels", "all")
  
  type AddOpenDuelsAmplitude = { Duels : string list }
  let addOpenDuelsLocation = ("open_duels", "add")
  let addOpenDuelsWave amplitude = (addOpenDuelsLocation, amplitude)
  
  let loginPopupNameChangeLocation = ("login_popup", "change_name_text")
  let loginPopupClickOkLocation = ("login_popup", "click_ok")
  
  let playDuelPopupNewButtonLocation = ("play_duel_popup", "new_button")
  let playDuelPopupJoinButtonLocation = ("play_duel_popup", "join_button")
  let playDuelPopupClickJoinLocation = ("play_duel_popup", "click_join")
  let playDuelPopupClickNewLocation = ("play_duel_popup", "click_new")
  
  let whitePlayerInformationPlateLocation = ("white_player", "information_plate")
  let blackPlayerInformationPlateLocation = ("black_player", "information_plate")
  let whitePlayerNameDynamicText = ("white_player", "name")
  let blackPlayerNameDynamicText = ("black_player", "name")
  
  let gameMenuLocation = ("game_menu", "menu")
  let gameMenuForfeitButtonLocation = ("game_menu", "forfeit_button")
  let gameMenuRemiseButtonLocation = ("game_menu", "remise_button")
  let gameMenuClickForfeitButtonLocation = ("game_menu", "click_forfeit")
  let gameMenuClickRemiseButtonLocation = ("game_menu", "click_remise")
  
  let confirmRemisePopupClickYesButtonLocation = ("confirm_remise_popup", "click_yes")
  let confirmRemisePopupClickNoButtonLocation = ("confirm_remise_popup", "click_no")
  
  let remiseRefusedPopupClickOkButtonLocation = ("remise_refused_popup", "click_ok")
  
  let duelStatePlateMenuLocation = ("duel_state_plate", "menu")
  let duelStatePlateRematchButtonLocation = ("duel_state_plate", "rematch_button")
  let duelStatePlateNewButtonLocation = ("duel_state_plate", "new_button")
  let duelStatePlateJoinButtonLocation = ("duel_state_plate", "join_button")
  let duelStatePlateClickRematchLocation = ("duel_state_plate", "click_rematch")
  let duelStatePlateClickNewLocation = ("duel_state_plate", "click_new")
  let duelStatePlateClickJoinLocation = ("duel_state_plate", "click_join")
  
  let confirmRematchPopupClickYesLocation = ("confirm_rematch_popup", "click_yes")
  let confirmRematchPopupClickNoLocation = ("confirm_rematch_popup", "click_no")
  let rematchRejectedPopupClickOkLocation = ("rematch_rejected_popup", "click_ok")

  type Amplitude =
  | DefaultAmplitude of DefaultAmplitude
  | TextAmplitude of TextAmplitude
  | UiComponentAmplitude of UiComponentAmplitude
  | UpdateTcpConnectionAmplitude of UpdateTcpConnectionAmplitude
  | UpdateUdpConnectionAmplitude of UpdateUdpConnectionAmplitude
  | AddTileAmplitude of AddTileAmplitude
  | LoginAmplitude of LoginAmplitude
  | PlayerCreatedAmplitude of PlayerCreatedAmplitude
  | ConfirmLoginAmplitude of ConfirmLoginAmplitude
  | FailedLoginAmplitude of FailedLoginAmplitude
  | ReportFailureAmplitude of ReportFailureAmplitude
  | NewDuelAmplitude of NewDuelAmplitude
  | StartDuelAmplitude of StartDuelAmplitude
  | JoinDuelAmplitude of JoinDuelAmplitude
  | AddDuelistAmplitude of AddDuelistAmplitude
  | RemoveDuelistAmplitude of RemoveDuelistAmplitude
  | AddPieceAmplitude of AddPieceAmplitude
  | RemovePieceAmplitude of RemovePieceAmplitude
  | MovePieceAmplitude of MovePieceAmplitude
  | ConquerTileAmplitude of ConquerTileAmplitude
  | PromotePieceAmplitude of PromotePieceAmplitude
  | UpdateBuffsAmplitude of UpdateBuffsAmplitude
  | SelectClientTileAmplitude of SelectClientTileAmplitude
  | SelectTileAmplitude of SelectTileAmplitude
  | ConfirmDeselectTileAmplitude of ConfirmDeselectTileAmplitude
  | OpenPopupAmplitude of OpenPopupAmplitude
  | ClosePopupAmplitude of ClosePopupAmplitude
  | AddOpenDuelsAmplitude of AddOpenDuelsAmplitude
  | UpdateDuelStateAmplitude of UpdateDuelStateAmplitude
  