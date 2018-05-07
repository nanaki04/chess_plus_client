namespace ChessPlus

open UnityEngine
open UnityEngine.UI
open Observers
open Flow
open Waves
open Result

type LoginView () =
  inherit Presentation ()
  
  let mutable unsubscribe = fun () -> ()
  
  member m.OnConnectionChange ({ Tcp = tcp; Udp = udp }) _ =
    match tcp, udp with
    | true, true ->
      flow <| requireLoginWave (DefaultAmplitude ())
      <!!> Logger.warn
    | _, _ -> ()
    
  override m.Awake () =
    base.Awake ()
    unsubscribe <- observeConnection m.OnConnectionChange
  
  override m.Update () =
    base.Update ()
   
  override m.OnDestroy () =
    base.OnDestroy ()
    unsubscribe ()