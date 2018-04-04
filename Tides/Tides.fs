namespace ChessPlus

module Tides =
  open Waves
  open Updaters
  
  let tides = [
    (addTileLocation, fun (_, amplitude) well ->
      let { Coordinate = coordinate; Tile = tile } = amplitude
      updateTile coordinate (fun _ -> tile) well
    )
  ]