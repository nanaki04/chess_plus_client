namespace ChessPlus

open UnityEngine

module ClickMeshEvents =
  type T = GameObject -> unit

  let private call = Agent.start (fun () -> (0, Map.empty))
  
  let register (event : T) =
    let id = ref 0
    call (fun (lastId, events) ->
      id := lastId + 1
      (!id, Map.add !id event events)
    )
    fun () -> call (fun (lastId, events) -> (lastId, Map.remove !id events))
    
  let fire gameObject =
    call (fun (lastId, events) ->
      Map.iter (fun _ event -> event gameObject) events
      (lastId, events)
    )
            
type InputHandlerView () =
  inherit Presentation ()
  
  [<SerializeField>]
  let mutable (mainCamera : Camera) = null
  
  override m.Update () =
    Input.GetMouseButtonUp (0)
    |> Option.fromBool
    |> Option.bind (fun _ -> Nullable.toOption mainCamera)
    |> Option.map (fun cam -> cam.ScreenPointToRay Input.mousePosition)
    |> Option.bind (fun ray ->
      let hit = ref (new RaycastHit ())
      Physics.Raycast (ray, hit, Mathf.Infinity)
      |> Option.fromBool
      |> Option.map (fun _ -> !hit)
    )
    |> Option.map (fun hit -> hit.collider.gameObject)
    |> Option.map ClickMeshEvents.fire
    |> ignore