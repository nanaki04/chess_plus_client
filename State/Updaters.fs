namespace ChessPlus

module Updaters =
  open Well
  open UnityEngine
  open Option
  
  let (<!>) = fun v f -> Option.map f v
  
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

  let updateBoard (updater : Board -> Board) (well : LifeWell) =
    well
    |> updateDuel (Option.map (fun d -> { d with Board = updater d.Board }))
    
  let updateTiles updater well =
    well
    |> updateBoard (fun b -> { b with Tiles = updater b.Tiles })

  let updateTile (row, column) updater well =
    (fun tiles ->
      let r = Map.tryFind row tiles |> function
        | Some r -> r
        | None -> Map.empty
      let tile = Map.tryFind column r |> function
        | Some t -> t
        | None -> Tile.initial
      Map.add column (updater tile) r
      |> Map.add row <| tiles
    )
    |> updateTiles <| well
    
  let updateOwnSelectedTile updater well =
    Finders.findOwnSelectedTileCoords well
    <!> fun coord -> updateTile coord updater well
    
  let updateOtherSelectedTile updater well =
    Finders.findOpponentSelectedTileCoords well
    <!> fun coord -> updateTile coord updater well
    
  let updateSelectedTile playerColor updater well =
    Finders.findSelectedTileCoord playerColor well
    <!> fun coord -> updateTile coord updater well
    |> Option.defaultValue well
    
  let updateOwnConquerableTiles updater well =
    Finders.findOwnConquerableTileCoords well
    <!> List.fold (fun w coord -> updateTile coord updater w) well
    |> Option.defaultValue well
    
  let updateOpponentConquerableTiles updater well =
    Finders.findOpponentConquerableTileCoords well
    <!> List.fold (fun w coord -> updateTile coord updater w) well
    |> Option.defaultValue well

  let updateConquerableTiles playerColor updater well =
    Finders.findConquerableTileCoords playerColor well
    <!> List.fold (fun w coord -> updateTile coord updater w) well
    |> Option.defaultValue well

  let updateSelections updater well =
    well
    |> updateBoard (fun b -> { b with Selections = updater b.Selections })
    
  let updateSelection playerColor updater well =
    match playerColor with
    | White ->
      updateSelections (fun s -> { s with White = updater s.White }) well
    | Black ->
      updateSelections (fun s -> { s with Black = updater s.Black }) well
      
  let updateOwnSelection updater well =
    Finders.findClientDuelist well
    <!> fun { Color = color } -> updateSelection color updater well
    |> Option.defaultValue well
    
  let updateOpponentSelection updater well =
    Finders.findOpponentDuelist well
    <!> fun { Color = color } -> updateSelection color updater well
    |> Option.defaultValue well
    
  let updateSelectionSelected playerColor updater well =
    updateSelection playerColor (fun s -> { s with Selected = updater s.Selected }) well
    
  let updateOwnSelectionSelected updater well =
    Finders.findClientDuelist well
    <!> fun { Color = color } -> updateSelectionSelected color updater well
    |> Option.defaultValue well
  
  let updateOpponentSelectionSelected updater well =
    Finders.findOpponentDuelist well
    <!> fun { Color = color } -> updateSelectionSelected color updater well
    |> Option.defaultValue well
    
  let updateSelectionConquerable playerColor updater well =
    updateSelection playerColor (fun s -> { s with Conquerable = updater s.Conquerable }) well
    
  let updateOwnSelectionConquerable updater well =
    Finders.findClientDuelist well
    <!> fun { Color = color } -> updateSelectionConquerable color updater well
    |> Option.defaultValue well
  
  let updateOpponentSelectionConquerable updater well =
    Finders.findOpponentDuelist well
    <!> fun { Color = color } -> updateSelectionConquerable color updater well 
    |> Option.defaultValue well
       
  let updatePiece coord updater well =
    well
    |> updateTile coord (fun t -> { t with Piece = updater t.Piece })
    
  let updateUi updater well =
    { well with Ui = updater well.Ui }
    
  let updatePopups updater well =
    well
    |> updateUi (fun ui -> { ui with Popups = updater ui.Popups })

  let updatePopupStates updater well =
    well
    |> updateUi (fun ui -> { ui with PopupStates = updater ui.PopupStates })
    
  let updateLoginPopupState updater well =
    well
    |> updatePopupStates (fun s -> { s with LoginPopupState = updater s.LoginPopupState })
    
  let updateUiComponents updater well =
    well
    |> updateUi (fun ui -> { ui with Components = updater ui.Components })
    
  let updateUiComponent location (updater : Option<UiComponent> -> Option<UiComponent>) well =
    let id = Types.Location.toString location
    well
    |> updateUiComponents (fun components ->
      Map.tryFind id components
      |> fun uiComponent -> updater uiComponent
      |> Option.map (fun uiComponent -> Map.add id uiComponent components)
      |> Option.defaultValue components
    )