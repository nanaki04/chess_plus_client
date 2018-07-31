namespace ChessPlus

module ConditionVerification =
  open Fetchers
  open Finders
  open Option
  open Types
  
  // MEMO: made use of mutable functions to realize recursion across modules
  // TODO find a better way
  let mutable canConquerBlackKing = fun (_ : WellCollection) -> false  
  let mutable canConquerWhiteKing = fun (_ : WellCollection) -> false
  let mutable canAnyConquerPiece = fun (_ : Pieces) (_ : WellCollection) -> false
  let mutable applyRule = fun (_ : Rule) (_ : Pieces option) (wellCollection : WellCollection) -> Ok wellCollection : Result<WellCollection, string>

  let private findCoordinatesToOffset (x, y) (row, column) =
    let rowInt = Row.toInt row
    let colInt = Column.toInt column
    let signX = sign x
    let signY = sign y
    let absX = abs x
    let absY = abs y
    let steps = (max absX absY) - 1
    let factorX = (float absX - 1.0) / float steps |> max 0.0
    let factorY = (float absY - 1.0) / float steps |> max 0.0    
    seq { 1..steps }
    |> Seq.map (fun step ->
      (float (step), float (step))
      |> fun (x, y) -> (factorX * x, factorY * y)
      |> fun (x, y) -> (round x, round y)
      |> fun (x, y) -> (int x, int y)
      |> fun (x, y) -> (signX * x, signY * y)
      |> fun (x, y) -> (rowInt + x, colInt + y)
      |> fun (x, y) -> (Row.fromInt x, Column.fromInt y)
      |> function
      | Ok row, Ok col -> Ok (row, col)
      | Error err, _ -> Error err
      | _, Error err -> Error err
    )
    |> Seq.toList
    |> Result.unwrap
      
  let private findCoordinatesInPath rule (piece : Pieces) =
    match rule, Types.Pieces.coord piece with
    | MoveRule { Offset = offset; }, Some coordinate ->
      findCoordinatesToOffset offset coordinate
    | ConquerRule { Offset = offset; }, Some coordinate ->
      findCoordinatesToOffset offset coordinate
    | MoveComboRule { MyOffset = offset; }, Some coordinate ->
      findCoordinatesToOffset offset coordinate
    | _, _ ->
      Ok []    

  let private isPathBlocked rule (piece : Pieces) wellCollection =
    let isBlocked (x, y) (row, column) pieceWell =
      let rowInt = Row.toInt row
      let colInt = Column.toInt column
      let signX = sign x
      let signY = sign y
      let absX = abs x
      let absY = abs y
      let steps = (max absX absY) - 1
      let factorX = (float absX - 1.0) / float steps |> max 0.0
      let factorY = (float absY - 1.0) / float steps |> max 0.0
      
      seq { 1..steps }
      |> Seq.map (fun step ->
        (float (step), float (step))
        |> fun (x, y) -> (factorX * x, factorY * y)
        |> fun (x, y) -> (round x, round y)
        |> fun (x, y) -> (int x, int y)
        |> fun (x, y) -> (signX * x, signY * y)
        |> fun (x, y) -> (rowInt + x, colInt + y)
        |> fun (x, y) -> (Row.fromInt x, Column.fromInt y)
        |> function
          | (Ok row, Ok col) -> Pool.isOccupied (row, col) pieceWell
          | _ -> false
      )
      |> Seq.tryFind id
      |> Option.defaultValue false
      |> Conditional
          
    match rule, Types.Pieces.coord piece, wellCollection with
    | MoveRule { Offset = offset; }, Some coordinate, { PieceWell = Some pieceWell } ->
      isBlocked offset coordinate pieceWell
    | ConquerRule { Offset = offset; }, Some coordinate, { PieceWell = Some pieceWell } ->
      isBlocked offset coordinate pieceWell
    | MoveComboRule { MyOffset = offset; Other = other; OtherOffset = otherOffset; }, Some coordinate, { PieceWell = Some pieceWell } ->
      match isBlocked offset coordinate pieceWell with
      | Conditional true -> Conditional true
      | Conditional false ->
        ((Types.Coordinate.applyOffset other coordinate
        |> Result.toOption)
        <!> fun otherCoord -> isBlocked otherOffset otherCoord pieceWell)
        |> Option.defaultValue (Conditional false)
      | _ -> Conditional false
    | _, None, _ -> // TODO why did I add this clause again ?
      Conditional true
    | _, _, _ ->
      Conditional false
      
  let private isOccupiedBy duelistType rule (piece : Pieces option) pieceWell =
    // TODO add support for multiple target checks for move combos
    match duelistType, Pool.Pieces.findTargetPiece rule piece pieceWell with
    | Any, Some piece ->
      Conditional true
      
    | Self, Some piece ->
      fetchClientDuelist ()
      <!> (fun self ->
        Types.Pieces.map (fun p ->
          p.Color = self.Color |> Conditional
        ) piece
      )
      |> Option.defaultValue (Conditional false)
      
    | DuelistType.Other, Some piece ->
      fetchOpponentDuelist ()
      <!> (fun other ->
        Types.Pieces.map (fun p ->
          p.Color = other.Color |> Conditional
        ) piece
      )
      |> Option.defaultValue (Conditional false)
      
    | Player color, Some piece ->
      Types.Pieces.map (fun p ->
        p.Color = color |> Conditional
      ) piece
      
    | _, _ ->
      Conditional false

  let isMet (operator, condition) rule piece isSimulation wellCollection =
    match condition, isSimulation, piece, wellCollection with
    | Always, _, _, _ -> 
      Conditional true
      |> Operators.isMet operator
      
    | PathBlocked, _, Some p, _ ->
      isPathBlocked rule p wellCollection
      |> Operators.isMet operator
      |> Result.map (Logger.inspect "isPathBlocked")

    | OccupiedBy _, true, _, _ ->
      // TODO too knowledgable and too specific
      Ok true
      
    | OccupiedBy duelist, false, _, { PieceWell = Some pieceWell } ->
      isOccupiedBy duelist rule piece pieceWell
      |> Operators.isMet operator
      
    | ExposesKing, true, _, _ ->
      Ok true
      
    | ExposesKing, false, _, { TileSelectionWell = Some tileSelectionWell; PieceWell = Some pieceWell; LifeWell = Some lifeWell } ->
      let simulateRule () =
        let simulate () =
          applyRule rule piece wellCollection
      
        match rule with
        | MoveRule r ->
          simulate ()
        | ConquerRule r ->
          simulate ()
        | _ ->
          Ok wellCollection
    
      match fetchClientDuelistColor () with
      | Some White ->
        simulateRule ()
        |> Result.map canConquerWhiteKing
        |> Result.map Conditional
        |> Result.bind (Operators.isMet operator)
      | Some Black ->
        simulateRule ()
        |> Result.map canConquerBlackKing
        |> Result.map Conditional
        |> Result.bind (Operators.isMet operator)
      | _ ->
        Ok true
     
    | ExposedWhileMoving, _, Some p, { PieceWell = Some pieceWell } ->
      findCoordinatesInPath rule p
      |> Result.map (List.fold (fun acc coord ->
        match acc with
        | true ->
          true
        | false ->
          Pool.Pieces.movePiece p coord pieceWell
          |> fun well -> { wellCollection with PieceWell = Some well; }
          |> canAnyConquerPiece (Types.Pieces.withCoord (Some coord) p)
      ) false)
      |> Result.map Conditional
      |> Result.bind (Operators.isMet operator)
            
    | MoveCount, _, Some p, _ ->
      Logger.warn "MoveCount"
      Types.Pieces.map (fun p -> p.MoveCount) p
      |> IntValue
      |> Logger.inspect "int value"
      |> Operators.isMet operator
      |> Logger.inspect "result"
      
    | MoveCount, _, _, _ ->
      Logger.warn "Invalid Move Count"
      Ok false
      
    | OtherPieceType pieceType, _, p, { PieceWell = Some pieceWell} ->
      Pool.Pieces.findOtherPieceType rule p pieceWell
      |> Option.map (fun otherPieceType -> pieceType = otherPieceType |> Conditional)
      |> Option.defaultValue (false |> Conditional)
      |> Operators.isMet operator
    
    | OtherOwner owner, _, p, { PieceWell = Some pieceWell} ->
      match owner, Pool.Pieces.findOtherPieceColor rule p pieceWell with
      | Any, Some _ ->
        Conditional true
        |> Operators.isMet operator
      | Self, Some c ->
        Pool.isPlayer c
        |> Conditional
        |> Operators.isMet operator
      | Other, Some c ->
        Pool.isPlayer c
        |> not
        |> Conditional
        |> Operators.isMet operator
      | Player playerColor, Some c ->
        playerColor = c
        |> Conditional
        |> Operators.isMet operator
      | _, _ ->
        Ok false
      
    | _ ->
      Ok false