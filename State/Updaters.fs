namespace ChessPlus

module Updaters =
  open Well
  open UnityEngine
  open Option
  open Fetchers
  
  let (<!>) = fun v f -> Option.map f v
  
  module LifeWell =
  
    let update updater well =
      updater well
      
    let updateConnection (updater : Connection -> Connection) (well : LifeWell) =
      { well with Connection = updater well.Connection }
      
    let updatePlayer (updater : Option<Player> -> Option<Player>) (well : LifeWell) =
      { well with Player = updater well.Player }
      
    let updateDuel (updater : Option<Duel> -> Option<Duel>) (well : LifeWell) =
      { well with Duel = updater well.Duel }
    
    let updateDuelists (updater : Duelist list -> Duelist list) (well : LifeWell) =
      well
      |> updateDuel (Option.map (fun d -> { d with Duelists = updater d.Duelists }))
    
    let updateDuelist color (updater : Duelist -> Duelist) (well : LifeWell) =
      well
      |> updateDuelists (List.map (fun d -> if d.Color = color then updater d else d))

  module Tiles =

    let updateTiles updater well =
      updater well

    let updateTile coord updater well =
      Map.update coord updater well
      
  module TileSelections =
  
    let updateSelections updater well =
      updater well
      
    let updateSelection playerColor updater well =
      match playerColor with
      | White ->
        updateSelections (fun s -> { s with White = updater s.White }) well
      | Black ->
        updateSelections (fun s -> { s with Black = updater s.Black }) well
        
    let updateOwnSelection updater well =
      fetchClientDuelist ()
      <!> fun { Color = color } -> updateSelection color updater well
      |> Option.defaultValue well
      
    let updateOpponentSelection updater well =
      fetchOpponentDuelist ()
      <!> fun { Color = color } -> updateSelection color updater well
      |> Option.defaultValue well
      
    let updateSelected playerColor updater well =
      updateSelection playerColor (fun s -> { s with Selected = updater s.Selected }) well
      
    let updateOwnSelected updater well =
      fetchClientDuelist ()
      <!> fun { Color = color } -> updateSelected color updater well
      |> Option.defaultValue well
    
    let updateOpponentSelectionSelected updater well =
      fetchOpponentDuelist ()
      <!> fun { Color = color } -> updateSelected color updater well
      |> Option.defaultValue well
      
    let updateSelectionConquerable playerColor updater well =
      updateSelection playerColor (fun s -> { s with Conquerable = updater s.Conquerable }) well
      
    let updateOwnSelectionConquerable updater well =
      fetchClientDuelist ()
      <!> fun { Color = color } -> updateSelectionConquerable color updater well
      |> Option.defaultValue well
    
    let updateOpponentSelectionConquerable updater well =
      fetchOpponentDuelist ()
      <!> fun { Color = color } -> updateSelectionConquerable color updater well 
      |> Option.defaultValue well
  
  module Pieces =     
  
    let updatePiece coord updater well =
      well
      |> Map.update coord updater
    
  module UI =  
    
    let updatePopups updater well =
      { well with Popups = updater well.Popups }
    
    let updatePopupStates updater well =
      { well with PopupStates = updater well.PopupStates }
      
    let updateLoginPopupState updater well =
      well
      |> updatePopupStates (fun s -> { s with LoginPopupState = updater s.LoginPopupState })
      
    let updateUiComponents updater well =
      { well with Components = updater well.Components }
      
    let updateUiComponent location (updater : Option<UiComponent> -> Option<UiComponent>) well =
      let id = Types.Location.toString location
      well
      |> updateUiComponents (fun components ->
        Map.tryFind id components
        |> fun uiComponent -> updater uiComponent
        |> Option.map (fun uiComponent -> Map.add id uiComponent components)
        |> Option.defaultValue components
      )