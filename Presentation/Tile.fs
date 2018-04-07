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

type TileView () =
  inherit Presentation ()

  [<SerializeField>]
  let mutable (image : Image) = null
        
  let mutable unsubscribe = fun () -> ()
  
  let changeColor color =
    Nullable.maybe (fun (img : Image) ->
      match color with
      | White -> img.color <- new Color(255.0f, 255.0f, 255.0f, 255.0f)
      | Black -> img.color <- new Color(0.0f, 0.0f, 0.0f, 255.0f)
    ) image

  member m.OnTileChange (tile : Option<Tile>) _ =
    tile
    |> Option.map (fun { Color = color } -> changeColor color)
    |> Option.orElseWith (fun () -> Some (GameObject.Destroy(m)))
    |> ignore
    
  override m.Start () =
    base.Start () 
    
  member m.Init row column color =
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