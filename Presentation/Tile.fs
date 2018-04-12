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
  let mutable (image : Image) = null
  
  let mutable coordinate = (One, A)
        
  let mutable unsubscribe = fun () -> ()
  
  let changeColor { Color = color; SelectedBy = selectedBy; ConquerableBy = conquerableBy } =
    let maybeImage = image |> Nullable.toOption
    match maybeImage, conquerableBy, selectedBy with
    | Some img, None, None ->
      img.color <- TileViewDefinitions.baseColors.[color]
    | Some img, Some playerColor, _ ->
      img.color <- TileViewDefinitions.conquerableColors.[playerColor].[color]
    | Some img, None, Some playerColor ->
      img.color <- TileViewDefinitions.selectedColors.[playerColor].[color]
    | _, _, _ ->
      ()

  member m.OnTileChange (tile : Option<Tile>) _ =
    tile
    |> Option.map changeColor
    |> Option.orElseWith (fun () -> Some (GameObject.Destroy(m)))
    |> ignore
    
  member m.OnClick () =
    flow <| deselectTileWave (DeselectTileAmplitude {
      Player = White;
    })
    <!!> Logger.warn
    
    flow <| selectTileWave (SelectTileAmplitude {
      Player = White;
      Coordinate = coordinate;
    })
    <!!> Logger.warn
    
  override m.Start () =
    base.Start () 
    
  member m.Init row column color =
    coordinate <- (row, column)
    unsubscribe <- observeTile (row, column) m.OnTileChange
    flow <| addTileWave (AddTileAmplitude {
      Coordinate = (row, column);
      Tile = Tile.create color None None None;
    })
    <!!> Logger.warn
    m
    
  override m.OnDestroy () =
    base.OnDestroy ()
    unsubscribe ()