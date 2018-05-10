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

type PlayerView () =
  inherit Presentation ()

  let mutable unsubscribe = fun () -> ()
  
  member m.OnPlayerChange player _ =
    player
    |> Option.map (fun _ ->
//      flow <| joinDuelWave (JoinDuelAmplitude {
//       ID = "127.0.0.1:5000"
//      })
      flow <| newDuelWave (NewDuelAmplitude {
        Map = Classic;
      })
      <!!> Logger.warn
    )
    |> ignore
  
  override m.Start () =
    base.Start ()
    unsubscribe <- observePlayer m.OnPlayerChange
    
  override m.OnDestroy () =
    base.OnDestroy ()
    unsubscribe ()