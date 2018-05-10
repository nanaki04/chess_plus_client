namespace ChessPlus

open UnityEngine
open UnityEngine.UI
open Flow
open Waves
open Adapters
open Result
open Tcp

type TcpStreamView () =
  inherit Presentation ()
  
  let mutable messages = Seq.empty
  let mutable state = Waiting
    
  override m.Start () =
    base.Start ()
    listen ()
    flow <| updateTcpConnectionWave (UpdateTcpConnectionAmplitude {
      Connected = true;
    })
    <!!> Logger.warn
  
  override m.Update () =
    base.Update ()
    
    match state with
    | Waiting ->
      state <- Receiving
      agent.Post (fun newMessages ->
        state <- Executing
        newMessages.Split '\n'
        |> Array.toSeq
        |> (fun msgList ->
          messages <- Seq.take (Seq.length msgList - 1) msgList
          Seq.reduce (fun _ x -> x) msgList
        )
      )
    | Executing ->
      Seq.iter Logger.log messages
      
      Seq.map (Moulds.import) messages
      |> Seq.map (Result.map flow >> Result.flatten)
      |> Seq.toList
      |> Result.unwrap
      <!!> Logger.warn
      
      state <- Waiting
    | Receiving ->
      ()
   
  override m.OnDestroy () =
    base.OnDestroy ()
    
    flow <| updateTcpConnectionWave (UpdateTcpConnectionAmplitude {
      Connected = false;
    })
    <!!> Logger.warn
        
    disconnect ()