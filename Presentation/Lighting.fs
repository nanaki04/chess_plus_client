namespace ChessPlus

open UnityEngine
open Observers

type LightingView () =
  inherit Presentation ()

  [<SerializeField>]
  let mutable (blackLight : Light) = null
  
  [<SerializeField>]
  let mutable (whiteLight : Light) = null
  
  let mutable unsubscribe = fun () -> ()
      
  member m.OnPlayerColorChanged playerColor _ =
    match playerColor with
    | Some Black ->
      blackLight.gameObject.SetActive true
      whiteLight.gameObject.SetActive false
    | Some White ->
      blackLight.gameObject.SetActive false
      whiteLight.gameObject.SetActive true
    | None ->
      blackLight.gameObject.SetActive false
      whiteLight.gameObject.SetActive false
    
  override m.Start () =
    base.Start ()
    unsubscribe <- observePlayerColor m.OnPlayerColorChanged
    
  override m.OnDestroy () =
    base.OnDestroy ()
    unsubscribe ()