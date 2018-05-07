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
  Rules : Rules;
}

type Connection = {
  Tcp : bool;
  Udp : bool;
}

type LifeWell = {
  Player : Option<Player>;
  Duel : Option<Duel>;
  Connection : Connection;
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
      
  module Rules =
    let create rules = rules
    
    let initial = Map.empty
   
  module Duel =
    let create duelists board rules =
      {
        Duelists = duelists;
        Board = board;
        Rules = rules;
      }
      
    let initial =
      {
        Duelists = [];
        Board = Board.initial;
        Rules = Rules.initial;
      }
      
  module Connection =
    let create tcp udp =
      {
        Tcp = tcp;
        Udp = udp;
      }
      
    let initial =
      {
        Tcp = false;
        Udp = false;
      }
      
  module LifeWell =
    let create player duel connection =
      {
        Player = player;
        Duel = duel;
        Connection = connection;
      }
      
    let initial =
      {
        Player = None;
        Duel = None;
        Connection = Connection.initial;
      }