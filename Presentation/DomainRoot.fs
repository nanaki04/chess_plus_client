namespace ChessPlus

open UnityEngine
open UnityEngine.UI
open Result
            
type DomainRootView () =
  inherit Presentation ()
  
  [<SerializeField>]
  let mutable domain : string = ""
  
  member m.Domain
    with get () = domain
  
  static member Find (gameObject : GameObject) =
    gameObject.GetComponentInParent<DomainRootView>()
    |> Nullable.toResult
    <!> fun domainRoot -> domainRoot.Domain