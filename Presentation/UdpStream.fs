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
open Udp
open System.Collections

type UdpStreamView () =
  inherit Presentation ()
  
  let mutable messages = []
  let mutable state = Waiting
    
  override m.Start () =
    base.Start ()
    connect ()
    
    flow <| updateUdpConnectionWave (UpdateUdpConnectionAmplitude {
      Connected = true;
    })
    <!!> Logger.warn
  
  override m.Update () =
    base.Update ()
    
    match state with
    | Waiting ->
      state <- Receiving
      agent.Post (fun newMessages ->
        messages <- newMessages
        state <- Executing
        []
      )
    | Executing ->
      List.iter Logger.log messages
      
      messages
      |> List.rev
      |> List.map (Moulds.import)
      |> List.map (Result.map flow >> Result.flatten)
      |> Result.unwrap
      <!!> Logger.warn
      
      state <- Waiting
    | Receiving ->
      ()
   
  override m.OnDestroy () =
    base.OnDestroy ()
    
    flow <| updateUdpConnectionWave (UpdateUdpConnectionAmplitude {
      Connected = false;
    })
    <!!> Logger.warn
    
    disconnect ()