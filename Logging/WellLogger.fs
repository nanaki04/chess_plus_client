namespace ChessPlus

module WellLogger =
  open UnityEngine
  open Logger
  open JsonConversions
  
  let private logWell =
    LifeWellDto.export
    >> export
    >> log
  
  let wellLogger next well = 
    log("<color='blue'>WELL BEFORE</color>")
    logWell well
    let refreshedWell = next well
    log("<color='green'>WELL AFTER</color>")
    logWell refreshedWell
    refreshedWell