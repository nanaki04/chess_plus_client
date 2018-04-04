﻿namespace ChessPlus

module Observers =
  open Well
  open Finders
  open Flow
  open Maelstrom.WellGuardians

  let observeBoard (react : Board -> LifeWell -> unit) =
    watcher findBoard react
    |> guard
    
  let observeTile coord (react : Tile -> LifeWell -> unit) =
    watcher (findTile coord) react
    |> guard
    
  let observe (react : LifeWell -> unit) =
    watcher find (fun well _ -> react well)
    |> guard