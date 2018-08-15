namespace ChessPlus

open Types
            
type ITileView =
  abstract member AddPiece : IPieceView -> unit
  abstract member HighlightAsSelected : Color -> ITileView
  abstract member HighlightAsConquerable : Color -> ITileView
  abstract member Init : Row -> Column -> Tile -> ITileView
  abstract member ResetColor : unit -> ITileView
  abstract member Destroy : unit -> unit