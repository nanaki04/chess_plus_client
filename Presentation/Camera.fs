namespace ChessPlus

open UnityEngine
open Observers

type CameraView () =
  inherit Presentation ()

  [<SerializeField>]
  let mutable (blackCamera : Camera) = null
  
  [<SerializeField>]
  let mutable (whiteCamera : Camera) = null
  
  let mutable unsubscribe = fun () -> ()
  
  let mutable (activeCamera : Option<Camera>) = None
      
  member m.OnPlayerColorChanged playerColor _ =
    match playerColor with
    | Some Black ->
      blackCamera.gameObject.SetActive true
      whiteCamera.gameObject.SetActive false
      activeCamera <- Some blackCamera
    | Some White ->
      blackCamera.gameObject.SetActive false
      whiteCamera.gameObject.SetActive true
      activeCamera <- Some whiteCamera
    | None ->
      blackCamera.gameObject.SetActive false
      whiteCamera.gameObject.SetActive false
      activeCamera <- None
     
  member m.ActiveCamera () =
    activeCamera
    
  override m.Start () =
    base.Start ()
    unsubscribe <- observePlayerColor m.OnPlayerColorChanged
    
  override m.OnDestroy () =
    base.OnDestroy ()
    unsubscribe ()