namespace ChessPlus

module WaveLogger =
  open UnityEngine
  open Logger
  open JsonConversions
  open Moulds
  
  let private logWave ((domain, invocation), _) =
    log ("<color='cyan'>FLOW: " + domain + ":" + invocation + "</color>")
  
  let waveLogger next wave =
    logWave wave
    next wave