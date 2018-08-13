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

module TileViewDefinitions =
  let private rgb r g b =
    new Color((float32 r) / 255.0f, (float32 g) / 255.0f, (float32 b) / 255.0f, 1.0f)

  let baseColors =
    Map.empty
      .Add(White, rgb 255 255 255)
      .Add(Black, rgb 0 0 0)
  
  let selectedColors =
    Map.empty
      .Add(White, Map.empty
        .Add(White, rgb 150 150 200)
        .Add(Black, rgb 100 100 150)
      )
      .Add(Black, Map.empty
        .Add(White, rgb 255 100 100)
        .Add(Black, rgb 150 0 0)
      )
      
  let movableColors =
    Map.empty
      .Add(White, Map.empty
        .Add(White, rgb 220 220 255)
        .Add(Black, rgb 50 50 100)
      )
      .Add(Black, Map.empty
        .Add(White, rgb 255 180 180)
        .Add(Black, rgb 75 0 0)
      )
      
  let conquerableColors =
    Map.empty
      .Add(White, Map.empty
        .Add(White, rgb 255 180 255)
        .Add(Black, rgb 100 50 100)
      )
      .Add(Black, Map.empty
        .Add(White, rgb 255 180 120)
        .Add(Black, rgb 150 50 0)
      )
            
type TileView () =
  inherit Presentation ()
      
  [<SerializeField>]
  let mutable image : Image = null

  let mutable piece = None
  let mutable coordinate = (One, A)
  let mutable color = White
        
  let mutable unsubscribe = fun () -> ()
    
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
      Nullable.toOption image
      |> Option.map (fun i -> i.color <- TileViewDefinitions.selectedColors.[playerColor].[color])
      |> Option.orFinally (fun () -> Logger.warn "TileView: No image set")
      m :> ITileView
      
    member m.HighlightAsConquerable playerColor =
      Nullable.toOption image
      |> Option.map (fun i -> i.color <- TileViewDefinitions.conquerableColors.[playerColor].[color])
      |> Option.orFinally (fun () -> Logger.warn "TileView: No image set")
      m :> ITileView
      
    member m.ResetColor () =
      Nullable.toOption image
      |> Option.map (fun i -> i.color <- TileViewDefinitions.baseColors.[color])
      |> Option.orFinally (fun () -> Logger.warn "TileView: No image set")
      m :> ITileView 
          
    member m.Init row column tile =
      coordinate <- (row, column)
      m.UpdateTile tile
      |> ignore
      m :> ITileView
      
    member m.Destroy () =
      GameObject.Destroy (m)