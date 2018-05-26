namespace ChessPlus

open UnityEngine
open UnityEngine.UI
open Observers
open Flow
open Waves
open Well
open Newtonsoft.Json
open Adapters
open DtoTypes
open JsonConversions
open Result
            
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
  
  let mutable _piece : GameObject = null
  let mutable _id : Piece option = None
 
  member private m.remove () =
    _id <- None
    
    Nullable.toOption _piece
    |> Option.map GameObject.Destroy
    |> ignore     
    
  member private m.spawn prefab data coord =
    if _id <> Some data then
      m.remove ()
      
      let piece = GameObject.Instantiate (prefab, m.transform) :?> GameObject
      _piece <- piece
      
      _id <- Some data
      
      piece.GetComponent<PieceView> ()
      |> fun (p : PieceView) -> p.Init data coord
      |> ignore
   
  override m.Start () =
    base.Start ()
    
  member m.Spawn piece coord =  
    match piece with
    | Some (Pawn p) -> m.spawn pawn p coord
    | Some (Knight p) -> m.spawn knight p coord
    | Some (Bishop p) -> m.spawn bishop p coord
    | Some (Rook p) -> m.spawn rook p coord
    | Some (Queen p) -> m.spawn queen p coord
    | Some (King p) -> m.spawn king p coord
    | None -> m.remove ()
     
  override m.OnDestroy () =
    base.OnDestroy ()
    Nullable.toOption _piece
    |> Option.map GameObject.Destroy
    |> ignore