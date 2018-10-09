namespace ChessPlus

open UnityEngine
open UnityEngine.UI
open Result
open Flow
open Waves
open Observers
            
type DynamicTextView () =
  inherit Presentation ()
  
  [<SerializeField>]
  let mutable invocation : string = ""
  
  [<SerializeField>]
  let mutable text : Text = null
  
  let mutable unsubscribe = fun () -> ()

  member private m.OnTextChange newText _ =
    Nullable.toResult text
    <!> (fun (text : Text) ->
      Option.map (fun newText -> text.text <- newText) newText
    )
    <!!> Logger.warn  
    
  member private m.Init domain invocation =
    unsubscribe <- observeDynamicText (domain, invocation) m.OnTextChange
    
    let uiWell = fetchUiWell ()
    let id = Types.Location.toString (domain, invocation)
    let text = Finders.findDynamicText id uiWell
    m.OnTextChange text uiWell
    
  override m.Start () =
    base.Start ()
    Ok m.Init
    <*> DomainRootView.Find(m.gameObject)
    <*> if invocation = "" then Error "No invocation set" else Ok invocation
    <!!> Logger.warn