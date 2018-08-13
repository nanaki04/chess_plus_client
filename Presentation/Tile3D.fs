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
  let mutable image : Image = null
  
  [<SerializeField>]
  let mutable blackMaterial : Material = null
  
  [<SerializeField>]
  let mutable whiteMaterial : Material = null  
  
  [<SerializeField>]
  let mutable blackSelectedMaterial : Material = null
  
  [<SerializeField>]
  let mutable whiteSelectedMaterial : Material = null
  
  [<SerializeField>]
  let mutable blackMovableMaterial : Material = null
  
  [<SerializeField>]
  let mutable whiteMovableMaterial : Material = null
  
  [<SerializeField>]
  let mutable blackConquerableMaterial : Material = null
  
  [<SerializeField>]
  let mutable whiteConquerableMaterial : Material = null
  
  [<SerializeField>]
  let mutable meshRenderer : MeshRenderer = null
  
  let mutable piece = None
  let mutable coordinate = (One, A)
  let mutable color = White
        
  let mutable unsubscribe = fun () -> ()
    
  member private m.SetPosition () =
    let (row, col) = coordinate
    Nullable.toOption meshRenderer
    |> Option.map (fun r ->
      new Vector3 (
        r.bounds.size.x * (Types.Row.toInt row |> float32),
        r.bounds.size.y * (Types.Column.toInt col |> float32),
        0.0f
      )
    )
    |> Option.map (fun pos -> m.gameObject.transform.position <- pos)
    |> Result.expectOption "Could not set tile position"
    <!!> Logger.warn
    
  member m.RemovePiece () =
    Option.map (fun (p : PieceView) -> p.Remove ()) piece
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
    
  override m.Start () =
    base.Start () 
    unsubscribe <- observeTile coordinate m.OnTileChange

  override m.OnDestroy () =
    base.OnDestroy ()
    unsubscribe ()
    
  interface ITileView with      
    member m.AddPiece (p : PieceView) =
      let add () =
        p.OnAddToTile m.MovePiece
        p.gameObject.transform.SetParent(m.gameObject.transform, false)
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
      match
        Nullable.toOption meshRenderer,
        Nullable.toOption whiteSelectedMaterial,
        Nullable.toOption blackSelectedMaterial,
        playerColor with
      | Some renderer, Some whiteMat, _, White ->
        renderer.materials.[0] <- whiteMat
      | Some renderer, _, Some blackMat, Black ->
        renderer.materials.[0] <- blackMat
      | _ ->
        ()
      m :> ITileView
      
    member m.HighlightAsConquerable playerColor =
      match
        Nullable.toOption meshRenderer,
        Nullable.toOption whiteConquerableMaterial,
        Nullable.toOption blackConquerableMaterial,
        playerColor with
      | Some renderer, Some whiteMat, _, White ->
        renderer.materials.[0] <- whiteMat
      | Some renderer, _, Some blackMat, Black ->
        renderer.materials.[0] <- blackMat
      | _ ->
        ()
      m :> ITileView
      
    member m.ResetColor () =
      match
        Nullable.toOption meshRenderer,
        Nullable.toOption whiteMaterial,
        Nullable.toOption blackMaterial,
        color with
      | Some renderer, Some whiteMat, _, White ->
        renderer.materials.[0] <- whiteMat
      | Some renderer, _, Some blackMat, Black ->
        renderer.materials.[0] <- blackMat
      | _ ->
        ()
      m :> ITileView
          
    member m.Init row column tile =
      coordinate <- (row, column)
      m.SetPosition ()
      m.UpdateTile tile
      
    member m.Destroy () =
      GameObject.Destroy (m)