namespace ChessPlus

open UnityEngine
open UnityEngine.UI
            
type PieceFactoryView () =
  inherit Presentation ()
      
  [<SerializeField>]
  let mutable (pawn : GameObject) = null
  
  [<SerializeField>]
  let mutable (rook : GameObject) = null
  
  [<SerializeField>]
  let mutable (bishop : GameObject) = null
  
  [<SerializeField>]
  let mutable (knight : GameObject) = null
  
  [<SerializeField>]
  let mutable (king : GameObject) = null
  
  [<SerializeField>]
  let mutable (queen : GameObject) = null
    
  member private m.spawn prefab (data : Pieces) coord =
      let piece = GameObject.Instantiate (prefab) :?> GameObject
      
      piece.GetComponent<PieceView> ()
      |> fun (p : PieceView) -> p.Init data coord
    
  member m.Spawn piece coord =
    match piece with
    | Pawn p -> m.spawn pawn piece coord
    | Knight p -> m.spawn knight piece coord
    | Bishop p -> m.spawn bishop piece coord
    | Rook p -> m.spawn rook piece coord
    | Queen p -> m.spawn queen piece coord
    | King p -> m.spawn king piece coord