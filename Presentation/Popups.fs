namespace ChessPlus

open UnityEngine
open UnityEngine.UI
open Observers
open Flow
open Waves
open Well
open Adapters
open Result
            
type PopupsView () =
  inherit Presentation ()
        
  let mutable unsubscribe = fun () -> ()
  
  let mutable openPopups : Map<Popup, GameObject> = Map.empty

  member m.Instantiate path =
    path
    |> Resources.Load
    |> fun obj -> (obj, m.transform)
    |> GameObject.Instantiate
    :?> GameObject
      
  member m.OnPopupsChange (popups : Popup list) _ =
    Map.partition (fun p _ -> List.contains p popups) openPopups
    |> fun (newOpenPopups, closedPopups) ->
      openPopups <- newOpenPopups
      Map.iter (fun _ p -> GameObject.Destroy p) closedPopups
      
    List.filter (fun p -> not (Map.containsKey p openPopups)) popups
    |> List.rev
    |> List.iter m.OpenPopup
    
  member m.OpenPopup (popup : Popup) =
    let obj =
      match popup with
      | Login ->
        m.Instantiate "Popup/LoginPopup"
      | PlayDuel ->
        m.Instantiate "Popup/PlayDuelPopup"
        
    openPopups <- Map.add popup obj openPopups
    
  override m.Awake () =
    base.Awake () 
    unsubscribe <- observePopups m.OnPopupsChange
    
  override m.OnDestroy () =
    base.OnDestroy ()
    unsubscribe ()