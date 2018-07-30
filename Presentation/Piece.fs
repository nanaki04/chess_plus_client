namespace ChessPlus

open UnityEngine
open UnityEngine.UI
open Result

module PieceViewDefinitions =
  let private rgb r g b =
    new Color((float32 r) / 255.0f, (float32 g) / 255.0f, (float32 b) / 255.0f, 1.0f)

  let baseColors =
    Map.empty
      .Add(White, rgb 255 255 255)
      .Add(Black, rgb 40 30 20)
            
type PieceView () =
  inherit Presentation ()
  
  [<SerializeField>]
  let mutable text : Text = null
  
  let mutable removeFromTile = fun () -> ()
  
  let mutable pieceType : string = ""
  
  let changeColor piece =
    let ({ Color = color } : Piece) = piece
    Nullable.toResult text
    <!> fun t -> t.color <- PieceViewDefinitions.baseColors.[color]
    <!!> Logger.warn
    
  member m.IsPieceType piece =
    Types.Pieces.toString piece = pieceType
  
  member m.Set piece =
    Types.Pieces.map changeColor piece
    m
    
  member m.Init (piece : Pieces) coord =
    Types.Pieces.map changeColor piece
    pieceType <- Types.Pieces.toString piece
    m
    
  member m.OnAddToTile remove =
    removeFromTile ()
    removeFromTile <- remove
    
  member m.Remove () =
    removeFromTile ()
    GameObject.Destroy m.gameObject