﻿namespace ChessPlus

type Player = {
  Name : Name;
}

type Duelist = {
  Color : Color;
  Name : Name;
}

type Tile = {
  Color : Color;
}

type Selection = {
  Selected : Coordinate option;
  Conquerable : Coordinate list;
}

type Duel = {
  Duelists : list<Duelist>;
}

type Connection = {
  Tcp : bool;
  Udp : bool;
}

type LoginPopupState = {
  Name : string;
}

type PopupStates = {
  LoginPopupState : LoginPopupState;
}

type UiComponent = {
  Visible : bool;
  Interactable : bool;
}

type UiComponents = Map<string, UiComponent>

type UiWell = {
  Popups : Popup list;
  PopupStates : PopupStates;
  Components : UiComponents;
}

type TileWell = Map<Coordinate, Tile>

type TileSelectionWell = {
  Black : Selection;
  White : Selection;  
}

type PieceWell = Map<Coordinate, Pieces>

type RuleWell = Rules

type LifeWell = {
  Player : Option<Player>;
  Duel : Option<Duel>;
  Connection : Connection;
}
  
module Well =

  type T =
  | LifeWell of LifeWell
  | RuleWell of RuleWell
  | PieceWell of PieceWell
  | TileSelectionWell of TileSelectionWell
  | TileWell of TileWell
  | UiWell of UiWell
  
  module Tile =
    let create color =
      {
        Color = color;
      }
      
    let initial =
      {
        Color = White;
      }
      
  module Selection =
    let create selected conquerable =
      {
        Selected = selected;
        Conquerable = conquerable;
      }
      
    let initial =
      {
        Selected = None;
        Conquerable = List.empty;
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
    let create duelists =
      {
        Duelists = duelists;
      }
      
    let initial =
      {
        Duelists = [];
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
      
  module LoginPopupState =
    let create name =
      {
        Name = name;
      }
      
    let initial =
      {
        Name = "";
      }
      
  module PopupStates =
    let create
      loginPopupState
      =
        {
          LoginPopupState = loginPopupState;
        }
  
    let initial =
      {
        LoginPopupState = LoginPopupState.initial;
      }
      
  module UiComponent =
    let create
      visible
      interactable
      =
        {
          Visible = visible;
          Interactable = interactable;
        }
        
    let initial =
      {
        Visible = true;
        Interactable = false;
      }
      
    let interactable v c =
      { c with Interactable = v }
      
    let visible v c =
      { c with Visible = v }
      
  module UiComponents =
    let initial = Map.empty
      
  module UiWell =
    let create popups popupStates components =
      {
        Popups = popups;
        PopupStates = popupStates;
        Components = components;
      }
      
    let initial =
      {
        Popups = [];
        PopupStates = PopupStates.initial;
        Components = UiComponents.initial;
      }
      
  module TileWell =
    let create tiles =
      tiles
    
    let initial : Map<Coordinate, Tile> =
      Map.empty
      
  module TileSelectionWell =
    let create black white =
      {
        Black = black;
        White = white;
      }
      
    let initial =
      {
        Black = Selection.initial;
        White = Selection.initial;
      }
      
  module PieceWell =
    let create pieces =
      pieces
      
    let initial : Map<Coordinate, Pieces> =
      Map.empty
      
  module RuleWell =
    let create rules =
      rules
      
    let initial : Map<int, Rule> =
      Map.empty
      
  module LifeWell =
    let create player duel connection ui =
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