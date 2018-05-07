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

type Column =
| A
| B
| C
| D
| E
| F
| G
| H

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
| AnyOf of Clause list
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

type Piece = {
  Color : Color;
  Rules : int;
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
      | _ -> Error ("Index out of range: " + x.ToString())
      
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
      
  module Row =
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
      
  module Piece =
    let create color =
      {
        Color = color;
        Rules = 0;
      }
      
  module Pieces =
    let map f p =
      match p with
      | King p -> f p
      | Queen p -> f p
      | Rook p -> f p
      | Bishop p -> f p
      | Knight p -> f p
      | Pawn p -> f p