namespace ChessPlus

module Pool =
  open Waves
  open Updaters
  open Fetchers
  open Finders
  open Option
  open Types
  
  let (<!>) = fun o f -> Option.map f o
    
  let isOccupied coord pieceWell =
    findPiece coord pieceWell
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
      
  let isPlayerTurn (well : LifeWell) =
    match well.Duel, findPlayerColor well with
    | Some { DuelState = Turn (Player color) }, Some playerColor ->
      color = playerColor
    | Some { DuelState = Turn Any }, Some playerColor ->
      true
    | _, _ ->
      false
        
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
      
    let findTargetPieceCoord rule (piece : Pieces option) well =
      match rule, piece with
      | MoveRule { Offset = offset; }, Some p ->
        Types.Pieces.coord p >>= findTarget offset
      | ConquerRule { Offset = offset; }, Some p ->
        Types.Pieces.coord p >>= findTarget offset
      | MoveComboRule { MyOffset = offset; }, Some p ->
        Types.Pieces.coord p >>= findTarget offset
      | ConquerComboRule { TargetOffset = targetOffset }, Some p ->
        Types.Pieces.coord p >>= findTarget targetOffset
      | _, _ ->
        None
     
    let findOtherPieceCoord rule (piece : Pieces option) well =
      match rule, piece with
      | MoveComboRule { Other = offset }, Some p ->
        Types.Pieces.coord p >>= findTarget offset
      | ConquerComboRule { TargetOffset = targetOffset }, Some p ->
        Types.Pieces.coord p >>= findTarget targetOffset
      | _, _ ->
        None   
        
    let findTargetPiece rule piece well =
      findTargetPieceCoord rule piece well
      >>= fun coord -> findPiece coord well

    let findOtherPiece rule piece well =
      findOtherPieceCoord rule piece well
      >>= fun coord -> findPiece coord well
      
    let findOtherPieceColor rule piece well =
      findOtherPiece rule piece well
      <!> Types.Pieces.color
      
    let findOtherPieceType rule piece well =
      findOtherPiece rule piece well
      <!> Types.Pieces.toString
      
    let findOtherPieceMovementCount rule piece well =
      findOtherPiece rule piece well
      <!> Types.Pieces.moveCount
                 
    let isPlayerPiece coord well =
      match findPiece coord well, fetchClientDuelist () with
      | Some p, Some { Color = playerColor } ->
        Types.Pieces.map (fun p -> p.Color = playerColor) p
      | _, _ ->
        false
        
    let movePiece piece toCoord well =
      Types.Pieces.map (fun p -> p.Coordinate) piece
      <!> (fun fromCoord ->
        updatePiece fromCoord (fun _ -> None) well
        |> updatePiece toCoord (fun _ ->
          Some (Types.Pieces.update (fun p ->
            { p with Coordinate = Some toCoord; MoveCount = p.MoveCount + 1 }
          ) piece)
        )
      )
      |> Option.defaultValue well

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