namespace ChessPlus

module Pool =
  open Waves
  open Updaters
  open Finders
  open Option
  
  let (<!>) = fun o f -> Option.map f o
  
  let deselect playerColor =
    updateSelectedTile playerColor (fun t -> { t with SelectedBy = None })
    >> updateSelectionSelected playerColor (fun _ -> None)
    
  let select coord playerColor =
    updateTile coord (fun t -> { t with SelectedBy = Some playerColor })
    >> updateSelectionSelected playerColor (fun _ -> Some coord)
    
  let isOccupied coord well =
    findTile coord well
    >>= fun tile -> tile.Piece
    |> function
      | Some _ -> true
      | _ -> false
      
  let isPlayer playerColor well =
    findClientDuelist well
    |> function
    | Some duelist -> duelist.Color = playerColor
    | _ -> false
    
  let isPlayerPiece coord well =
    match findPiece coord well, findClientDuelist well with
    | Some p, Some { Color = playerColor } ->
      Types.Pieces.map (fun p -> p.Color = playerColor) p
    | _, _ ->
      false
      
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