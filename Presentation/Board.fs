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
  let mutable (rowCount : int) = 0
  [<SerializeField>]
  let mutable (columnCount : int) = 0
  [<SerializeField>]
  let mutable (tile : GameObject) = null
        
  let mutable unsubscribe = fun () -> ()
  
  member private m.MakeTile row column color =
    Nullable.maybe (fun (obj : GameObject) ->
      GameObject.Instantiate (obj, m.transform) :?> GameObject
      |> (fun obj -> obj.GetComponent<TileView>())
      |> (fun tileView -> tileView.Init row column color)
      |> ignore
    ) tile

  member private m.MakeRow row =
    [1..(columnCount + 1)]
    |> List.map (fun col ->
      let color = if (row + col) % 2 = 0 then White else Black
      Ok (fun row col -> m.MakeTile row col color)
      <*> (RowDto.import row)
      <*> (Types.Column.fromInt col)
      <!!> Logger.warn
    )

  member private m.MakeBoard () =
    [1..(rowCount + 1)]
    |> List.map m.MakeRow
    
  override m.Start () =
    base.Start ()  
    m.MakeBoard () |> ignore
    
  override m.OnDestroy () =
    base.OnDestroy ()
    unsubscribe ()