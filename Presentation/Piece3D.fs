namespace ChessPlus

open UnityEngine
open UnityEngine.UI
open Result
            
type Piece3DView () =
  inherit Presentation ()
  
  [<SerializeField>]
  let mutable blackMaterial : Material = null
  
  [<SerializeField>]
  let mutable whiteMaterial : Material = null
  
  [<SerializeField>]
  let mutable meshRenderer : MeshRenderer = null  
  
  let mutable removeFromTile = fun () -> ()
  
  let mutable pieceType : string = ""
  
  let changeColor piece =
    let ({ Color = color } : Piece) = piece
    match
      piece,
      Nullable.toOption meshRenderer,
      Nullable.toOption blackMaterial,
      Nullable.toOption whiteMaterial with
    | { Color = White }, Some renderer, _, Some mat ->
      renderer.material <- mat
    | { Color = Black }, Some renderer, Some mat, _ ->
      renderer.material <- mat
    | _ ->
      ()
    
  interface IPieceView with
    member m.IsPieceType piece =
      Types.Pieces.toString piece = pieceType
    
    member m.Set piece =
      Types.Pieces.map changeColor piece
      m :> IPieceView
      
    member m.Init (piece : Pieces) coord =
      Types.Pieces.map changeColor piece
      pieceType <- Types.Pieces.toString piece
      m :> IPieceView
      
    member m.OnAddToTile remove =
      removeFromTile ()
      removeFromTile <- remove
      
    member m.Remove () =
      removeFromTile ()
      GameObject.Destroy m.gameObject
      
    member m.AddToParent parent =
      m.gameObject.transform.SetParent (parent, false)