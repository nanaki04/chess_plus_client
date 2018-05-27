namespace ChessPlus

module Flow =
  open Well
  open Maelstrom.Mealstrom
  open Tides
  open WellLogger
  open WaveLogger
  
  type Wave = ((string * string) * Maelstrom.Amplitude<Maelstrom.Amplitude<Waves.Amplitude>>)
  
  let (doFlow, guard) = invoke LifeWell.initial tides [waveLogger] [wellLogger]
  let flow wave = doFlow wave |> Ok
  
//  let mutable private queue : (unit -> unit) list = []
//  
//  let rec private next () =  
//    if queue.Length > 0 then
//      let command = List.head queue
//      command ()
//      queue <- List.tail queue
//      next ()
//    else
//      Ok ()
//      
//  let enqueue command =
//    queue <- command::(List.rev queue) |> List.rev
//    if queue.Length = 1 then next () else Ok ()
//            
//  let flow wave =
//    let command = fun () ->
//      Logger.log "HANDLE WAVE"
//      Logger.log (JsonConversions.export (Moulds.LocationMould.export (Tuple.fst wave)))
//      maelstrom <- flow wave maelstrom
//      
//    enqueue command
//    
//  let guard wellGuardian =
//    let (unguard, mstr) = guard wellGuardian maelstrom
//    maelstrom <- mstr
//    fun () ->
//      maelstrom <- unguard maelstrom