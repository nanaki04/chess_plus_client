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

type BoardView () =
  inherit Presentation ()

  [<SerializeField>]
  let mutable (tile : GameObject) = null
  
  member private m.MakeTile row column t =
    Nullable.maybe (fun (obj : GameObject) ->
      GameObject.Instantiate (obj, m.transform) :?> GameObject
      |> (fun obj -> obj.GetComponent<TileView>())
      |> (fun tileView -> tileView.Init row column t)
      |> ignore
    ) tile
    
  member m.Init (board : Board) =
    Matrix.mapRC (fun r c t ->
      m.MakeTile r c t
    ) board.Tiles
    |> ignore
    m