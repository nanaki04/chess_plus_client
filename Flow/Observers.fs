namespace ChessPlus

module Observers =
  open Well
  open Finders
  open Flow
  open Maelstrom.WellGuardians
  
  let observeConnection (react : Connection -> LifeWell -> unit) =
    watcher findConnection react
    |> guard
    
  let observePlayer (react : Option<Player> -> LifeWell -> unit) =
    watcher findPlayer react
    |> guard
    
  let observeDuel (react : Option<Duel> -> LifeWell -> unit) =
    watcher findDuel react
    |> guard
    
  let observeDuelState (react : Option<DuelState> -> LifeWell -> unit) =
    watcher findDuelState react
    |> guard
    
  let observePlayerColor (react : Option<Color> -> LifeWell -> unit) =
    watcher findPlayerColor react
    |> guard
    
  let observeTiles (react : TileWell -> TileWell -> unit) =
    watcher id react
    |> guardTileWell
    
  let observeTile coord (react : Option<Tile> -> TileWell -> unit) =
    watcher (findTile coord) react
    |> guardTileWell
    
  let observePieces (react : PieceWell -> PieceWell -> unit) =
    watcher id react
    |> guardPieceWell
    
  let observePiece coord (react : Option<Pieces> -> PieceWell -> unit) =
    watcher (findPiece coord) react
    |> guardPieceWell
    
  let observePopups (react : Popup list -> UiWell -> unit) =
    watcher findPopups react
    |> guardUiWell
    
  let observeUiComponent location (react : Option<UiComponent> -> UiWell -> unit) =
    let id = Types.Location.toString location
    watcher (findUiComponent id) react
    |> guardUiWell
  
  let observeSelections (react : TileSelectionWell -> TileSelectionWell -> unit) =
    watcher id react
    |> guardTileSelectionWell
    
  let observe (react : LifeWell -> unit) =
    watcher find (fun well _ -> react well)
    |> guard