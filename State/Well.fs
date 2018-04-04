namespace ChessPlus

type Tile = {
  Color : Color
}
  
type Board = {
  Tiles : Map<Row, Map<Column, Tile>>
}

type LifeWell = {
  Board : Board
}
  
module Well =
  
  module Tile =
    let create color =
      {
        Color = color;
      }
      
    let initial =
      {
        Color = White;
      }
 
  module Board =
    let create tiles =
      {
        Tiles = tiles;
      }
      
    let initial =
      {
        Tiles = Map.empty;
      }
   
  module LifeWell =
    let create board =
      {
        Board = board;
      }
      
    let initial =
      {
        Board = Board.initial
      }
  
  // depricated
  let initialTile = {
    Color = White
  }
  
  let initialBoard = {
    Tiles = Map.empty
  } 
    
  let initialLifeWell = {
    Board = initialBoard
  }