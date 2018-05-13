namespace ChessPlus

open UnityEngine
open UnityEngine.UI
open Result
open Flow
open Waves
            
type InputView () =
  inherit Presentation ()
  
  [<SerializeField>]
  let mutable invocation : string = ""
  
  [<SerializeField>]
  let mutable inputField : InputField = null
  
  member m.OnTextChange text =
    DomainRootView.Find m.gameObject
    <!> fun domain -> flow ((domain, invocation), TextAmplitude { Text = text; })
    <!!> Logger.warn
    
  override m.Start () =
    base.Start ()
    Nullable.toResult inputField
    <!> fun i -> i.onValueChanged.AddListener (fun t -> m.OnTextChange t)
    <!!> Logger.warn