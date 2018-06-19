namespace ChessPlus

module ConditionVerification =
  open Fetchers
  open Finders
  open Option
  open Types
  
  // MEMO: made use of mutable functions to realize recursion across modules
  // TODO find a better way
  let mutable canConquerBlackKing = fun (_ : PieceWell) -> false  
  let mutable canConquerWhiteKing = fun (_ : PieceWell) -> false

  let private isPathBlocked rule well =
    let isBlocked (x, y) (row, column) =
      let rowInt = Row.toInt row
      let colInt = Column.toInt column
      let signX = sign x
      let signY = sign y
      let absX = abs x
      let absY = abs y
      let steps = (max absX absY) - 1
      let factorX = (float absX - 1.0) / float steps |> max 0.0
      let factorY = (float absY - 1.0) / float steps |> max 0.0
      
      Seq.init steps (fun step ->
        (float step, float step)
        |> fun (x, y) -> (factorX * x, factorY * y)
        |> fun (x, y) -> (round x, round y)
        |> fun (x, y) -> (int x, int x)
        |> fun (x, y) -> (signX * x, signY * y)
        |> fun (x, y) -> (rowInt + x, colInt + y)
        |> fun (x, y) -> (Row.fromInt x, Column.fromInt y)
        |> function
          | (Ok row, Ok col) -> Pool.isOccupied (row, col)
          | _ -> false
      )
      |> Seq.tryFind id
      |> Option.defaultValue false
      |> Conditional
          
    match rule, (findOwnSelectedTileCoords well (fetchLifeWell ())) with
    | MoveRule { Offset = offset; }, Some coordinate ->
      isBlocked offset coordinate
    | ConquerRule { Offset = offset; }, Some coordinate ->
      isBlocked offset coordinate
    | _, _ ->
      Conditional false
      
  let private isOccupiedBy duelistType rule tileSelectionWell =
    match duelistType, Pool.TileSelections.findTargetPiece rule tileSelectionWell with
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
      
    | Other, Some piece ->
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

  let isMet (operator, condition) rule isSimulation tileSelectionWell =
    match condition, isSimulation with
    | Always, _ -> 
      Conditional true
      |> Operators.isMet operator
      
    | PathBlocked, _ ->
      isPathBlocked rule tileSelectionWell
      |> Operators.isMet operator
      
    | OccupiedBy duelist, _ ->
      isOccupiedBy duelist rule tileSelectionWell
      |> Operators.isMet operator
      
    | ExposesKing, true ->
      Ok true
      
    | ExposesKing, false ->
      match fetchClientDuelistColor () with
      | Some White ->
        canConquerWhiteKing (fetchPieceWell ())
        |> Conditional
        |> Operators.isMet operator
      | Some Black ->
        canConquerBlackKing (fetchPieceWell ())
        |> Conditional
        |> Operators.isMet operator
      | _ ->
        Ok true
            
    | MoveCount, _ ->
      IntValue 2 // TODO
      |> Operators.isMet operator
      
    | _ ->
      Ok false