namespace ChessPlus

open UnityEngine
open UnityEngine.UI
open Observers
open Result
open Flow
open Waves
open Finders
            
type DuelStateView () =
  inherit Presentation ()
  
  [<SerializeField>]
  let mutable messageBoard : Text = null
  
  let mutable unsubscribe = fun () -> ()
  
  let mutable active = false
  
  let timer = async {
    active <- true
    do! Async.Sleep 1000
    active <- false
  }

  member m.DisplayDuelResult endedState (msgBoard : Text) well =
    match endedState with
    | Remise ->
      msgBoard.text <- "Remise"
      active <- true
    | Win color ->
      msgBoard.text <- if findPlayerColor well = Some color then "Victory" else "Defeat"
      active <- true
      
  member m.DisplayNextTurn duelist (msgBoard : Text) well =
    match duelist with
    | Player color ->
      Logger.logUnionName color
      msgBoard.text <- if findPlayerColor well = Some color then "Your turn" else "Enemy turn"
      Async.Start timer
    | _ ->
      ()
  
  member m.OnChangeDuelState duelState well =
    match duelState, Nullable.toOption messageBoard with
    | Some (Turn duelist), Some msgBoard ->
      m.DisplayNextTurn duelist msgBoard well
    | Some Paused, Some _ ->
      ()
    | Some (Ended endedState), Some msgBoard ->
      m.DisplayDuelResult endedState msgBoard well
    | _, _ ->
      ()
    
  override m.Start () =
    base.Start ()   
    unsubscribe <- Observers.observeDuelState m.OnChangeDuelState
    
  override m.OnDestroy () =
    base.OnDestroy ()   
    unsubscribe ()
    
  override m.Update () =
    base.Update ()
    Nullable.toOption messageBoard
    |> Option.map(fun msgBoard -> msgBoard.gameObject.SetActive(active))
    |> ignore
    ()
    