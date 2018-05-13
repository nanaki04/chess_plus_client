namespace ChessPlus

open UnityEngine
open UnityEngine.UI
open Result
open Flow
open Waves
            
type ButtonView () =
  inherit Presentation ()
  
  [<SerializeField>]
  let mutable invocation : string = ""
  
  [<SerializeField>]
  let mutable button : Button = null
  
  member m.OnClick () =
    DomainRootView.Find (m.gameObject)
    <!> (fun domain ->    
      flow ((domain, invocation), DefaultAmplitude ())
    )
    <!!> Logger.warn
    
  override m.Start () =
    base.Start ()
    Nullable.toResult button
    <!> fun b -> b.onClick.AddListener (fun () -> m.OnClick ())
    <!!> Logger.warn