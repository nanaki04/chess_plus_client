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
  let mutable applyRule = fun (_ : Rule) (_ : Pieces option) (wellCollection : WellCollection) -> Ok wellCollection : Result<WellCollection, string>

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
    | _, None ->
      Conditional true
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

    | OccupiedBy _, true ->
      // TODO too knowledgable and too specific
      Ok true
      
    | OccupiedBy duelist, false ->
      isOccupiedBy duelist rule tileSelectionWell
      |> Operators.isMet operator
      
    | ExposesKing, true ->
      Ok false
      
    | ExposesKing, false ->
      let simulateRule pieceWell =
        let simulate () =
          let wellCollection = { Well.WellCollection.initial with PieceWell = Some pieceWell }
          findOwnSelectedPiece tileSelectionWell pieceWell (fetchLifeWell ())
          |> (fun p -> applyRule rule p wellCollection)
          |> function
          | Ok { PieceWell = Some pw } -> pw
          | _ -> pieceWell 
      
        match rule with
        | MoveRule r ->
          simulate ()
        | ConquerRule r ->
          simulate ()
        | _ ->
          pieceWell
    
      match fetchClientDuelistColor () with
      | Some White ->
        Logger.warn "Exposes white king"
        
        canConquerWhiteKing (simulateRule (fetchPieceWell ()))
        |> Conditional
        |> Operators.isMet operator
      | Some Black ->
        Logger.warn "Exposes black king"
        
        canConquerBlackKing (simulateRule (fetchPieceWell ()))
        |> Conditional
        |> Operators.isMet operator
      | _ ->
        Logger.warn "ExposesKing invalid color"
        Ok true
            
    | MoveCount, _ ->
      IntValue 2 // TODO
      |> Operators.isMet operator
      
    | _ ->
      Ok false