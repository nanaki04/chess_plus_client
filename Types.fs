namespace ChessPlus

type Color =
| White
| Black

type Name = string

type Row =
| One
| Two
| Three
| Four
| Five
| Six
| Seven
| Eight
| Nine
| Ten
| Eleven
| Twelve

type Column =
| A
| B
| C
| D
| E
| F
| G
| H
| I
| J
| K
| L

type Coordinate = Row * Column

type DuelistType =
| Any
| Self
| Other
| Player of Color

type Condition =
| Always
| MoveCount
| ExposesKing
| PathBlocked
| OccupiedBy of DuelistType
| Conquerable
| Movable
| Defendable
| OtherPieceType of string
| OtherOwner of DuelistType
| ExposedWhileMoving
| Row of Row
| Column of Column
| RemainingPieceTypes of string list

type Operator =
| Is
| Not
| Equals of int
| GreaterThan of int
| SmallerThan of int

type Clause = Operator * Condition

type Conditions =
| Clause of Clause
| AllOf of Clause list
| OneOf of Clause list
| Combination of Conditions list

type MoveRule = {
  Condition : Conditions;
  Offset : int * int;
}

type ConquerRule = {
  Condition : Conditions;
  Offset : int * int;
}

type MoveComboRule = {
  Condition : Conditions;
  Other : int * int;
  MyOffset : int * int;
  OtherOffset : int * int;
}

type PromoteRule = {
  Condition : Conditions;
  Ranks : int;
}

type DefeatRule = {
  Condition : Conditions;
}

type RemiseRule = {
  Condition : Conditions;
}

type Rule =
| MoveRule of MoveRule
| ConquerRule of ConquerRule
| MoveComboRule of MoveComboRule
| PromoteRule of PromoteRule
| DefeatRule of DefeatRule
| RemiseRule of RemiseRule

type Rules = Map<int, Rule>

type PieceID = int

type Piece = {
  ID : PieceID;
  Color : Color;
  Rules : int list;
  MoveCount : int;
  Coordinate : Coordinate option;
}

type King = Piece
type Queen = Piece
type Rook = Piece
type Bishop = Piece
type Knight = Piece
type Pawn = Piece

type Pieces =
| King of King
| Queen of Queen
| Rook of Rook
| Bishop of Bishop
| Knight of Knight
| Pawn of Pawn

type Territory =
| Classic

type StreamState =
| Waiting
| Receiving
| Executing

type Location = string * string

type ConditionResult =
| Conditional of bool
| IntValue of int
| StringValue of string

module Types =
  module Column =
    let fromInt x =
      match x with
      | 1 -> Ok A
      | 2 -> Ok B
      | 3 -> Ok C
      | 4 -> Ok D
      | 5 -> Ok E
      | 6 -> Ok F
      | 7 -> Ok G
      | 8 -> Ok H
      | 9 -> Ok I
      | 10 -> Ok J
      | 11 -> Ok K
      | 12 -> Ok L
      | _ -> Error ("Column index out of range: " + x.ToString())
      
    let toInt x =
      match x with
      | A -> 1
      | B -> 2
      | C -> 3
      | D -> 4
      | E -> 5
      | F -> 6
      | G -> 7
      | H -> 8
      | I -> 9
      | J -> 10
      | K -> 11
      | L -> 12
      
  module Row =
    let fromInt x =
      match x with
      | 1 -> Ok One
      | 2 -> Ok Two
      | 3 -> Ok Three
      | 4 -> Ok Four
      | 5 -> Ok Five
      | 6 -> Ok Six
      | 7 -> Ok Seven
      | 8 -> Ok Eight
      | 9 -> Ok Nine
      | 10 -> Ok Ten
      | 11 -> Ok Eleven
      | 12 -> Ok Twelve
      | _ -> Error ("Row index out of range: " + x.ToString())
  
    let toInt x =
      match x with
      | One -> 1
      | Two -> 2
      | Three -> 3
      | Four -> 4
      | Five -> 5
      | Six -> 6
      | Seven -> 7
      | Eight -> 8
      | Nine -> 9
      | Ten -> 10
      | Eleven -> 11
      | Twelve -> 12
      
  module Coordinate =
    let fromInt (x, y) =
      match Row.fromInt x, Column.fromInt y with
      | Ok row, Ok column -> Ok (row, column)
      | Error e, _ -> Error e
      | _, Error e -> Error e

    let toInt (row, column) =
      (Row.toInt row, Column.toInt column)
      
    let def =
      (One, A)
      
    let getOffset coord1 coord2 =
      let (x1, y1) = toInt coord1
      let (x2, y2) = toInt coord2
      (x2 - x1, y2 - y1)
      
    let applyOffset (x, y) coord =
      toInt coord
      |> (fun (r, c) -> (r + x, c + y))
      |> fromInt

  module Condition =
    let toString c =
      match c with
      | Always -> "Always"
      | MoveCount -> "MoveCount"
      | ExposesKing -> "ExposesKing"
      | PathBlocked -> "PathBlocked"
      | OccupiedBy _ -> "OccupiedBy"
      | Conquerable -> "Conquerable"
      | Movable -> "Movable"
      | Defendable -> "Defendable"
      | OtherOwner _ -> "OtherOwner"
      | OtherPieceType _ -> "OtherPieceType"
      | ExposedWhileMoving -> "ExposedWhileMoving"
      | Row _ -> "Row"
      | Column _ -> "Column"
      | RemainingPieceTypes _ -> "RemainingPieceTypes"
      
  module Piece =
    let create id color rules coordinate moveCount =
      {
        ID = id;
        Color = color;
        Rules = rules;
        Coordinate = coordinate;
        MoveCount = moveCount;
      }
      
  module Pieces =
    let map (f : Piece -> 'a) p =
      match p with
      | King p -> f p
      | Queen p -> f p
      | Rook p -> f p
      | Bishop p -> f p
      | Knight p -> f p
      | Pawn p -> f p
      
    let update (f : Piece -> Piece) p =
      match p with
      | King p -> f p |> King
      | Queen p -> f p |> Queen
      | Rook p -> f p |> Rook
      | Bishop p -> f p |> Bishop
      | Knight p -> f p |> Knight
      | Pawn p -> f p |> Pawn
      
    let coord p =
      map (fun p -> p.Coordinate) p
      
    let withCoord coord p =
      update (fun p -> { p with Coordinate = coord; }) p
      
    let color p =
      map (fun p -> p.Color) p
      
    let rules p =
      map (fun p -> p.Rules) p
      
    let id p =
      map (fun p -> p.ID) p
      
    let toString p =
      match p with
      | King _ -> "King"
      | Queen _ -> "Queen"
      | Rook _ -> "Rook"
      | Bishop _ -> "Bishop"
      | Knight _ -> "Knight"
      | Pawn _ -> "Pawn"
      
  module Location =
    let toString (d, i) =
      d + ":" + i
      
  module MoveRule =
    let map f r =
      match r with
      | MoveRule r -> Some (f r)
      | _ -> None
      
  module ConquerRule =
    let map f r =
      match r with
      | ConquerRule r -> Some (f r)
      | _ -> None