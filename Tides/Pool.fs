namespace ChessPlus

module Pool =
  open Waves
  open Updaters
  open Fetchers
  open Finders
  open Option
  open Types
  
  let (<!>) = fun o f -> Option.map f o
    
  let isOccupied coord =
    fetchPiece coord
    |> function
      | Some _ -> true
      | _ -> false
      
  let isPlayer playerColor =
    fetchClientDuelist ()
    |> function
    | Some duelist ->
      duelist.Color = playerColor
    | _ ->
      false
      
  let private findTarget (x, y) (row, column) =
     (Row.toInt row, Column.toInt column)
     |> fun (r, c) -> (r + x, c + y)
     |> fun (r, c) -> (Row.fromInt r, Column.fromInt c)
     |> function
       | (Ok row, Ok col) ->
         Some (row, col)
       | _ ->
         None 
    
  let findTargetCoord rule =
    match rule, fetchOwnSelectedTileCoords () with
    | MoveRule { Offset = offset; }, Some coordinate ->
      findTarget offset coordinate
    | ConquerRule { Offset = offset; }, Some coordinate ->
      findTarget offset coordinate
    | _, _ ->
      None
        
  module TileSelections =
    open Updaters.TileSelections
  
    let deselect playerColor well =
      updateSelected playerColor (fun _ -> None) well
      
    let select coord playerColor well =
      updateSelected playerColor (fun _ -> Some coord) well
      
    let findTargetCoord rule well =
      match rule, findOwnSelectedTileCoords well (fetchLifeWell ()) with
      | MoveRule { Offset = offset; }, Some coordinate ->
        findTarget offset coordinate
      | ConquerRule { Offset = offset; }, Some coordinate ->
        findTarget offset coordinate
      | _, _ ->
        None
      
    let findTargetPiece rule well =
      findTargetCoord rule well
      >>= fetchPiece

  module Pieces =
    open Updaters.Pieces
      
    let findTargetPiece rule well =
      findTargetCoord rule
      >>= fun c -> findPiece c well
                 
    let isPlayerPiece coord well =
      match findPiece coord well, fetchClientDuelist () with
      | Some p, Some { Color = playerColor } ->
        Types.Pieces.map (fun p -> p.Color = playerColor) p
      | _, _ ->
        false

  module UI =
    open Updaters.UI
      
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