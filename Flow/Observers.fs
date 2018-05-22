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

  let observeBoard (react : Option<Board> -> LifeWell -> unit) =
    watcher findBoard react
    |> guard
    
  let observeTile coord (react : Option<Tile> -> LifeWell -> unit) =
    let (row, col) = coord
    watcher (findTile coord) react
    |> guard
    
  let observePiece coord (react : Option<Pieces> -> LifeWell -> unit) =
    watcher (findPiece coord) react
    |> guard
    
  let observePopups (react : Popup list -> LifeWell -> unit) =
    watcher findPopups react
    |> guard
    
  let observeUiComponent location (react : Option<UiComponent> -> LifeWell -> unit) =
    let id = Types.Location.toString location
    watcher (findUiComponent id) react
    |> guard
    
  let observe (react : LifeWell -> unit) =
    watcher find (fun well _ -> react well)
    |> guard