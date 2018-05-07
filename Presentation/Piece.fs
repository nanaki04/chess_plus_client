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

module PieceViewDefinitions =
  let private rgb r g b =
    new Color((float32 r) / 255.0f, (float32 g) / 255.0f, (float32 b) / 255.0f, 1.0f)

  let baseColors =
    Map.empty
      .Add(White, rgb 255 255 255)
      .Add(Black, rgb 0 0 0)
            
type PieceView () =
  inherit Presentation ()
  
  [<SerializeField>]
  let mutable text : Text = null

  let mutable coordinate = (One, A)
        
  let mutable unsubscribe = fun () -> ()
  
  let changeColor piece =
    let ({ Color = color } : Piece) = piece
    Nullable.toResult text
    <!> fun t -> t.color <- PieceViewDefinitions.baseColors.[color]
    <!!> Logger.warn
    piece
  
  member m.OnPieceChange piece _ =
    piece
    |> changeColor
    |> ignore
    
  override m.Start () =
    base.Start () 
    
  member m.Init piece coord =
    coordinate <- coord
    unsubscribe <- observePiece coord (fun p _ ->
      Option.map (Types.Pieces.map m.OnPieceChange) p
      |> ignore
    )
    m
    
  override m.OnDestroy () =
    base.OnDestroy ()
    unsubscribe ()