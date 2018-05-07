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
  
  let mutable messages = Seq.empty
  let mutable readUntil = 0
    
  override m.Start () =
    base.Start ()
    connect ()
    
    listen (fun msg ->
      messages <- Seq.skip readUntil messages |> Seq.append [msg]
      readUntil <- 0
    )
    
    flow <| updateUdpConnectionWave (UpdateUdpConnectionAmplitude {
      Connected = true;
    })
    <!!> Logger.warn
  
  override m.Update () =
    base.Update ()
    Seq.iter Logger.log (Seq.skip readUntil messages)
    
    Seq.map (Moulds.import) (Seq.skip readUntil messages)
    |> Seq.map (Result.map flow >> Result.flatten)
    |> Seq.toList
    |> Result.unwrap
    <!!> Logger.warn
    
    readUntil <- Seq.length messages
   
  override m.OnDestroy () =
    base.OnDestroy ()
    
    flow <| updateUdpConnectionWave (UpdateUdpConnectionAmplitude {
      Connected = false;
    })
    <!!> Logger.warn
    
    disconnect ()