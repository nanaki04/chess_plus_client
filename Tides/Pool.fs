namespace ChessPlus

module Pool =
  open Waves
  open Updaters
  open Finders
  open Option
  
  let (<!>) = fun o f -> Option.map f o
  
  let deselect playerColor well =
    updateTiles (Matrix.updateWhere
      (fun t -> t.SelectedBy = Some playerColor)
      (fun t -> { t with SelectedBy = None })
    ) well
    
  let findClientDuelist well =
    match findPlayer well, findDuelists well with
    | Some player, Some duelists ->
      List.tryFind (fun (d : Duelist) -> d.Name = player.Name) duelists
    | _, _ ->
      None