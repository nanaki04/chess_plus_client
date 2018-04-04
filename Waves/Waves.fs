namespace ChessPlus

module Waves =
  open Well
  
  type AddTileAmplitude = { Coordinate : Coordinate; Tile : Tile }
  let addTileLocation = ("tile", "add")
  let addTileWave amplitude = (addTileLocation, amplitude)

  type Amplitude =
  | AddTileAmplitude of AddTileAmplitude