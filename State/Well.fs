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

type Ui = {
  Popups : Popup list;
  PopupStates : PopupStates;
  Components : UiComponents;
}

type LifeWell = {
  Player : Option<Player>;
  Duel : Option<Duel>;
  Connection : Connection;
  Ui : Ui;
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
      
  module Ui =
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
      
  module LifeWell =
    let create player duel connection ui =
      {
        Player = player;
        Duel = duel;
        Connection = connection;
        Ui = ui;
      }
      
    let initial =
      {
        Player = None;
        Duel = None;
        Connection = Connection.initial;
        Ui = Ui.initial;
      }