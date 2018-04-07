namespace ChessPlus


type Player = {
  Name : Name;
}

type Duelist = {
  Color : Color;
  Name : Name;
}

type Tile = {
  Color : Color;
  Piece : Option<Pieces>;
  SelectedBy : Option<Color>;
  ConquerableBy : Option<Color>;
}
  
type Board = {
  Tiles : Map<Row, Map<Column, Tile>>
}

type Duel = {
  Duelists : list<Duelist>;
  Board : Board;
}

type LifeWell = {
  Player : Player;
  Duel : Option<Duel>;
}
  
module Well =
  
  module Tile =
    let create color piece selectedBy conquerableBy =
      {
        Color = color;
        Piece = piece;
        SelectedBy = selectedBy;
        ConquerableBy = conquerableBy;
      }
      
    let initial =
      {
        Color = White;
        Piece = None;
        SelectedBy = None;
        ConquerableBy = None;
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
      
  module Player =
    let create name : Player =
      {
        Name = name;
      }
      
    let initial : Player =
      {
        Name = "";
      }
      
  module Duelist =
    let create name color =
      {
        Name = name;
        Color = color;
      }
      
    let initial =
      {
        Name = "";
        Color = White;
      }
   
  module Duel =
    let create duelists board =
      {
        Duelists = duelists;
        Board = board;
      }
      
    let initial =
      {
        Duelists = [];
        Board = Board.initial;
      }
      
  module LifeWell =
    let create player duel =
      {
        Player = player;
        Duel = duel;
      }
      
    let initial =
      {
        Player = Player.initial;
        Duel = None;
      }