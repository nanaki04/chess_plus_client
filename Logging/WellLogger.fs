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
    log("WELL BEFORE")
    logWell well
    let refreshedWell = next well
    log("WELL AFTER")
    logWell refreshedWell
    refreshedWell