namespace ChessPlus

open UnityEngine
open UnityEngine.UI
open Result
open Flow
open Waves
open Observers
open JsonConversions
open DtoTypes
            
type UiComponentView () =
  inherit Presentation ()
  
  [<SerializeField>]
  let mutable invocation : string = ""
  
  [<SerializeField>]
  let mutable state : UiComponentDto = new UiComponentDto ()
  
  let mutable unsubscribe = fun () -> ()
  
  member m.Render uiComponent =  
    m.gameObject.GetComponent<Button>()
    |> Nullable.toResult
    <!> fun b -> b.interactable <- uiComponent.Interactable
    |> ignore
    
    m.gameObject.SetActive uiComponent.Visible
  
  member m.OnComponentChange uiComponent _ =
    uiComponent
    |> Option.iter (fun c -> state <- UiComponentDto.export c)
    |> Option.iter m.Render
    |> Option.orFinally (fun () -> GameObject.Destroy(m))
    ()
  
  member private m.Init domain invocation =
    unsubscribe <- observeUiComponent (domain, invocation) m.OnComponentChange
    
    Logger.log (Types.Location.toString (domain, invocation))
    Logger.warn state
    UiComponentDto.import state
    <!> (fun uiComponent ->
      flow ((domain, invocation), UiComponentAmplitude uiComponent)
    )
    
  override m.Start () =
    base.Start ()
    Ok m.Init
    <*> DomainRootView.Find(m.gameObject)
    <*> if invocation = "" then Error "No invocation set" else Ok invocation
    <!!> Logger.warn