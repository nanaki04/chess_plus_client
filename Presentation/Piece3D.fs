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
  
  member m.ChangeColor (piece : Piece) =
    match
      piece,
      Nullable.toOption meshRenderer,
      Nullable.toOption blackMaterial,
      Nullable.toOption whiteMaterial with
    | { Color = White }, Some renderer, _, Some mat ->
      renderer.material <- mat
      m.gameObject.transform.localEulerAngles <- new Vector3 (0.0f, 0.0f, 180.0f)
    | { Color = Black }, Some renderer, Some mat, _ ->
      renderer.material <- mat
      m.gameObject.transform.localEulerAngles <- new Vector3 (0.0f, 0.0f, 0.0f)
    | _ ->
      ()
    
  interface IPieceView with
    member m.IsPieceType piece =
      Types.Pieces.toString piece = pieceType
    
    member m.Set piece =
      Types.Pieces.map m.ChangeColor piece
      m :> IPieceView
      
    member m.Init (piece : Pieces) _coord =
      Types.Pieces.map m.ChangeColor piece
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