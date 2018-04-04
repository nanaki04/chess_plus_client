namespace ChessPlus

module Flow =
  open Well
  open Maelstrom.Mealstrom
  open Tides
  open WellLogger
  
  let mutable private maelstrom = invoke initialLifeWell tides List.empty [wellLogger]
  
  let flow wave =
    maelstrom <- flow wave maelstrom
    UnityEngine.Debug.Log("FLOW REFLECTIONS")
    List.map (fun refl ->
      UnityEngine.Debug.Log(refl)
      refl
    ) maelstrom.reflection
    |> ignore
    Ok ()
    
  let guard wellGuardian =
    let (unguard, mstr) = guard wellGuardian maelstrom
    maelstrom <- mstr
    fun () ->
      maelstrom <- unguard maelstrom