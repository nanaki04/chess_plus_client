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

type Piece = {
  Color : Color;
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
      
  module Piece =
    let create color =
      {
        Color = color;
      }