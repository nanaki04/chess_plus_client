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
    
  let isOccupied coord well =
    findTile coord well
    >>= fun tile -> tile.Piece
    |> function
      | Some _ -> true
      | _ -> false
      
  let openPopup popup well =
    updatePopups (fun popups -> popup::popups) well
    
  let closePopup popup well =
    updatePopups (fun popups ->
      List.filter (fun p -> p <> popup) popups
    ) well
    
  module UiComponent =
    let init location well =
      updateUiComponent location (fun _ -> Some Well.UiComponent.initial) well
      
    let set location state well =
      updateUiComponent location (fun _ -> Some state) well
      
    let update location f well =
      updateUiComponent location (Option.map (fun c ->
        f c
      )) well
      
    let interactable location v well =
      update location (fun c ->
        { c with Interactable = v }
      ) well
      
    let visible location v well =
      update location (fun c ->
        { c with Visible = v }
      ) well