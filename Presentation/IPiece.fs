namespace ChessPlus

open Types
open UnityEngine
            
type IPieceView =
  abstract member IsPieceType : Pieces -> bool
  abstract member Set : Pieces -> IPieceView
  abstract member Init : Pieces -> Coordinate -> IPieceView
  abstract member OnAddToTile : (unit -> unit) -> unit
  abstract member Remove : unit -> unit
  abstract member AddToParent : Transform -> unit