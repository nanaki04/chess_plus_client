namespace ChessPlus

type ConditionResult =
| Conditional of bool
| IntValue of int

module ConditionVerification =
  open Finders
  open Option
  open Types

  let private isPathBlocked rule well =
    let timer = Logger.time "isPathBlocked"
    let isBlocked (x, y) (row, column) =
      let isBlockedTimer = Logger.time "isBlocked"
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
        let seqTimer = Logger.time "Init Seq"
        (float step, float step)
        |> fun (x, y) -> (factorX * x, factorY * y)
        |> fun (x, y) -> (round x, round y)
        |> fun (x, y) -> (int x, int x)
        |> fun (x, y) -> (signX * x, signY * y)
        |> fun (x, y) -> (rowInt + x, colInt + y)
        |> fun (x, y) -> (Row.fromInt x, Column.fromInt y)
        |> function
          | (Ok row, Ok col) -> Pool.isOccupied (row, col) well
          | _ -> false
        |> seqTimer
      )
      |> Seq.tryFind id
      |> Option.defaultValue false
      |> Conditional
      |> isBlockedTimer
          
    match rule, (findSelectedTile well) with
    | MoveRule { Offset = offset; }, Some (coordinate, _) ->
      isBlocked offset coordinate
      |> timer
    | ConquerRule { Offset = offset; }, Some (coordinate, _) ->
      isBlocked offset coordinate
      |> timer
    | _, _ ->
      Conditional false
      |> timer
      
  let private isOccupiedBy duelistType rule well =
    let timer = Logger.time "isOccupiedBy"
    match duelistType, findTargetPiece rule well with
    | Any, Some piece ->
      Conditional true
      |> timer
      
    | Self, Some piece ->
      let occBySelfTimer = Logger.time "occBySelf"
      findClientDuelist well
      <!> (fun self ->
        Types.Pieces.map (fun p ->
          p.Color = self.Color |> Conditional
        ) piece
      )
      |> Option.defaultValue (Conditional false)
      |> timer
      |> occBySelfTimer
      
    | Other, Some piece ->
      let occupiedByOtherTimer = Logger.time "occByOther"
      findClientDuelist well
      <!> (fun self ->
        Types.Pieces.map (fun p ->
          p.Color <> self.Color |> Conditional
        ) piece
      )
      |> Option.defaultValue (Conditional false)
      |> timer
      |> occupiedByOtherTimer
      
    | Player color, Some piece ->
      let occupiedByAnyTimer = Logger.time "occByAny"
      Types.Pieces.map (fun p ->
        p.Color = color |> Conditional
      ) piece
      |> timer
      |> occupiedByAnyTimer
      
    | _, _ ->
      Conditional false
      |> timer

  let isMet condition rule well =
    let timer = Logger.time "isMet"
    match condition with
    | Always -> 
      Conditional true
      |> timer
      
    | PathBlocked ->
      Conditional false
      // isPathBlocked rule well
      |> timer
      
    | OccupiedBy duelist ->
      Conditional false
      // isOccupiedBy duelist rule well
      |> timer
      
    | ExposesKing ->
      Conditional false // TODO
      |> timer
            
    | MoveCount ->
      IntValue 2 // TODO
      |> timer
      
    | _ ->
      Conditional true
      |> timer