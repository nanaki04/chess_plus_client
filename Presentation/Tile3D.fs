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
            
type Tile3DView () =
  inherit Presentation ()
  
  [<SerializeField>]
  let mutable blackMaterial : Material = null
  
  [<SerializeField>]
  let mutable whiteMaterial : Material = null  
  
  [<SerializeField>]
  let mutable blackSelectedBlackTileMaterial : Material = null
  
  [<SerializeField>]
  let mutable blackSelectedWhiteTileMaterial : Material = null  
  
  [<SerializeField>]
  let mutable whiteSelectedBlackTileMaterial : Material = null
  
  [<SerializeField>]
  let mutable whiteSelectedWhiteTileMaterial : Material = null 
   
  [<SerializeField>]
  let mutable blackConquerableBlackTileMaterial : Material = null
  
  [<SerializeField>]
  let mutable blackConquerableWhiteTileMaterial : Material = null  
  
  [<SerializeField>]
  let mutable whiteConquerableBlackTileMaterial : Material = null
  
  [<SerializeField>]
  let mutable whiteConquerableWhiteTileMaterial : Material = null  
  
  [<SerializeField>]
  let mutable meshRenderer : MeshRenderer = null
  
  let mutable piece = None
  let mutable coordinate = (One, A)
  let mutable color = White
        
  let mutable unsubscribe = fun () -> ()
  let mutable unsubscribeClickMeshEvent = fun () -> ()
    
  member private m.SetPosition () =
    let (row, col) = coordinate
    Nullable.toOption meshRenderer
    |> Option.map (fun r ->
      new Vector3 (
        r.bounds.size.x * (Types.Row.toInt row |> float32),
        0.0f,
        r.bounds.size.z * (Types.Column.toInt col |> float32)
      )
    )
    |> Option.map (fun pos -> m.gameObject.transform.position <- pos)
    |> Result.expectOption "Could not set tile position"
    <!!> Logger.warn
 
  member m.AdjustColor blackMat whiteMat =
    match Nullable.toOption meshRenderer, Nullable.toOption blackMat, Nullable.toOption whiteMat, color with
    | Some r, Some mat, _, Black ->
      r.material <- mat
    | Some r, _, Some mat, White ->
      r.material <- mat
    | _ ->
      ()
    
  member m.AdjustColorToSelected playerColor =
    match playerColor with
    | Black ->  
      m.AdjustColor blackSelectedBlackTileMaterial blackSelectedWhiteTileMaterial
    | White ->
      m.AdjustColor whiteSelectedBlackTileMaterial whiteSelectedWhiteTileMaterial

  member m.AdjustColorToConquerable playerColor =
    match playerColor with
    | Black ->
      m.AdjustColor blackConquerableBlackTileMaterial blackConquerableWhiteTileMaterial
    | White ->
      m.AdjustColor whiteConquerableBlackTileMaterial whiteConquerableWhiteTileMaterial
          
  member m.RemovePiece () =
    Option.map (fun (p : IPieceView) -> p.Remove ()) piece
    |> ignore
    
  member m.MovePiece () =
    piece <- None

  member m.OnTileChange (tile : Option<Tile>) _ =
    tile
    |> Option.map m.UpdateTile
    |> Option.orFinally (fun () -> GameObject.Destroy(m))
    
  member m.UpdateTile (tile : Tile) =
    color <- tile.Color
    (m :> ITileView).ResetColor ()
    
  member m.OnClick () =
    flow <| selectClientTileWave (SelectClientTileAmplitude {
      Coordinate = coordinate;
    })
    <!!> Logger.warn
    
  member m.OnClickMesh obj =
    m.gameObject = obj
    |> Option.fromBool
    |> Option.map m.OnClick
    |> ignore
    
  override m.Start () =
    base.Start () 
    unsubscribe <- observeTile coordinate m.OnTileChange
    unsubscribeClickMeshEvent <- ClickMeshEvents.register m.OnClickMesh

  override m.OnDestroy () =
    base.OnDestroy ()
    unsubscribe ()
    unsubscribeClickMeshEvent ()
    
  interface ITileView with      
    member m.AddPiece (p : IPieceView) =
      let add () =
        p.OnAddToTile m.MovePiece
        p.AddToParent(m.gameObject.transform)
        piece <- Some p
        
      match piece with
      | Some current ->
        if (current <> p)
        then
          m.RemovePiece ()
          add ()
      | None ->
        add ()
    
    member m.HighlightAsSelected playerColor =
      m.AdjustColorToSelected playerColor
      m :> ITileView
      
    member m.HighlightAsConquerable playerColor =
      m.AdjustColorToConquerable playerColor
      m :> ITileView
      
    member m.ResetColor () =
      m.AdjustColor blackMaterial whiteMaterial
      m :> ITileView
          
    member m.Init row column tile =
      coordinate <- (row, column)
      m.SetPosition ()
      m.UpdateTile tile
      
    member m.Destroy () =
      GameObject.Destroy (m)