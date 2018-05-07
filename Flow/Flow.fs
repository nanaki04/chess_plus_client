namespace ChessPlus

module Flow =
  open Well
  open Maelstrom.Mealstrom
  open Tides
  open WellLogger
  
  let mutable private maelstrom = invoke LifeWell.initial tides List.empty [wellLogger]
  
  let flow wave =
    Logger.log (JsonConversions.export (Moulds.LocationMould.export (Tuple.fst wave)))
    maelstrom <- flow wave maelstrom
    Logger.log "FLOW REFLECTIONS"
    List.map (fun refl ->
      Logger.log refl
      refl
    ) maelstrom.reflection
    |> ignore
    Ok ()
    
  let guard wellGuardian =
    let (unguard, mstr) = guard wellGuardian maelstrom
    maelstrom <- mstr
    fun () ->
      maelstrom <- unguard maelstrom