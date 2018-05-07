namespace ChessPlus

open UnityEngine
open UnityEngine.UI
open Logger

type public Presentation () =
  inherit MonoBehaviour ()

  abstract member Awake : unit -> unit
  abstract member Start : unit -> unit
  abstract member Update : unit -> unit
  abstract member OnDestroy : unit -> unit
  
  default m.Awake () =
    log(m.GetType().ToString() + ": Awake")
  default m.Start () =
    log(m.GetType().ToString() + ": Start")
  default m.Update () =
    ()
    // TODO collapsed log or something ?
    // log(m.GetType().ToString() + ": Update")
  default m.OnDestroy () =
    log(m.GetType().ToString() + ": OnDestroy")