namespace ChessPlus

open System.Collections.Generic

module DtoTypes =  
  type ColorDto = string
     
  type RowDto = int      
  type ColumnDto = string      
  type CoordinateDto = string
  
  type DuelistTypeDto () =
    let mutable ``type`` = null
    let mutable player = Unchecked.defaultof<ColorDto>
    
    member m.Type
      with get () = ``type``
      and set (v) = ``type`` <- v
    member m.Player
      with get () = player
      and set (v) = player <- v
  
  type ConditionDto () =
    let mutable ``type`` = null
    let mutable occupiedBy = Unchecked.defaultof<DuelistTypeDto>
    let mutable otherPieceType = Unchecked.defaultof<string>
    let mutable otherOwner = Unchecked.defaultof<DuelistTypeDto>
    let mutable row = 0
    let mutable column = 0
    
    member m.Type
      with get () = ``type``
      and set (v) = ``type`` <- v
    member m.OccupiedBy
      with get () = occupiedBy
      and set (v) = occupiedBy <- v
    member m.OtherPieceType
      with get () = otherPieceType
      and set (v) = otherPieceType <- v
    member m.OtherOwner
      with get () = otherOwner
      and set (v) = otherOwner <- v
    member m.Row
      with get () = row
      and set (v) = row <- v
    member m.Column
      with get () = column
      and set (v) = column <- v
            
  type OperatorDto () =
    let mutable ``type`` = null
    let mutable value = Unchecked.defaultof<int>
    let mutable stringValue = Unchecked.defaultof<string>
    
    member m.Type
      with get () = ``type``
      and set (v) = ``type`` <- v
    member m.Value
      with get () = value
      and set (v) = value <- v
    member m.StringValue
      with get () = stringValue
      and set (v) = stringValue <- v
      
  type ClauseDto () =
    let mutable operator = Unchecked.defaultof<OperatorDto>
    let mutable condition = Unchecked.defaultof<ConditionDto>
    
    member m.Operator
      with get () = operator
      and set (v) = operator <- v
    member m.Condition
      with get () = condition
      and set (v) = condition <- v
      
  type ConditionsDto () =
    let mutable ``type`` = null
    let mutable clause = Unchecked.defaultof<ClauseDto>
    let mutable clauses = Unchecked.defaultof<ClauseDto array>
    let mutable combination = Unchecked.defaultof<ConditionsDto array>
    
    member m.Type
      with get () = ``type``
      and set (v) = ``type`` <- v
    member m.Clause
      with get () = clause
      and set (v) = clause <- v
    member m.Clauses
      with get () = clauses
      and set (v) = clauses <- v
    member m.Combination
      with get () = combination
      and set (v) = combination <- v
           
  type RuleDto () =
    let mutable ``type`` = null
    let mutable condition = Unchecked.defaultof<ConditionsDto>
    let mutable other = Unchecked.defaultof<int array>
    let mutable offset = Unchecked.defaultof<int array>
    let mutable myMovement = Unchecked.defaultof<int array>
    let mutable otherMovement = Unchecked.defaultof<int array>
    let mutable ranks = Unchecked.defaultof<int>
    
    member m.Type
      with get () = ``type``
      and set (v) = ``type`` <- v
    member m.Condition
      with get () = condition
      and set (v) = condition <- v
    member m.Other
      with get () = other
      and set (v) = other <- v
    member m.Offset
      with get () = offset
      and set (v) = offset <- v
    member m.MyMovement
      with get () = myMovement
      and set (v) = myMovement <- v
    member m.OtherMovement
      with get () = otherMovement
      and set (v) = otherMovement <- v
    member m.Ranks
      with get () = ranks
      and set (v) = ranks <- v
      
  type RulesDto = IDictionary<int, RuleDto>
  
  type PieceDto () =
    let mutable ``type`` = Unchecked.defaultof<string>
    let mutable id = Unchecked.defaultof<int>
    let mutable color = Unchecked.defaultof<ColorDto>
    let mutable rules = Array.empty
    let mutable coordinate = Unchecked.defaultof<CoordinateDto>
    let mutable moveCount = Unchecked.defaultof<int>

    member m.Type
      with get () = ``type``
      and set (v) = ``type`` <- v
    member m.Id
      with get () = id
      and set (v) = id <- v
    member m.Color
      with get () = color
      and set (v) = color <- v
    member m.Rules
      with get () = rules
      and set (v) = rules <- v
    member m.Coordinate
      with get () = coordinate
      and set (v) = coordinate <- v
    member m.MoveCount
      with get () = moveCount
      and set (v) = moveCount <- v

  type TileDto () =
    let mutable color = Unchecked.defaultof<ColorDto>

    member m.Color
      with get () = color
      and set (v) = color <- v
      
  type SelectionDto () =
    let mutable selected = Unchecked.defaultof<CoordinateDto>
    let mutable conquerable = Array.empty
    
    member m.Selected
      with get () = selected
      and set (v) = selected <- v
    member m.Conquerable
      with get () = conquerable
      and set (v) = conquerable <- v
      
  type SelectionsDto () =
    let mutable black = Unchecked.defaultof<SelectionDto>
    let mutable white = Unchecked.defaultof<SelectionDto>
    
    member m.Black
      with get () = black
      and set (v) = black <- v
    member m.White
      with get () = white
      and set (v) = white <- v

  type PlayerDto = {
    Name : string;
  }
  
  type DuelistDto = {
    Name : string;
    Color : ColorDto;
  }
  
  type EndedStateDto () =
    let mutable ``type`` = Unchecked.defaultof<string>
    let mutable value = Unchecked.defaultof<ColorDto>
    
    member m.Type
      with get () = ``type``
      and set (v) = ``type`` <- v
    member m.Value
      with get () = value
      and set (v) = value <- v
  
  type DuelStateDto () =
    let mutable ``type`` = Unchecked.defaultof<string>
    let mutable turn = Unchecked.defaultof<DuelistTypeDto>
    let mutable ended = Unchecked.defaultof<EndedStateDto>
    
    member m.Type
      with get () = ``type``
      and set (v) = ``type`` <- v
    member m.Turn
      with get () = turn
      and set (v) = turn <- v
    member m.Ended
      with get () = ended
      and set (v) = ended <- v
  
  type TerritoryDto = string
  
  type DuelDto () =
    let mutable duelists = Array.empty
    let mutable duelState = Unchecked.defaultof<DuelStateDto>
    
    member m.Duelists
      with get () = duelists
      and set (v) = duelists <- v
    member m.DuelState
      with get () = duelState
      and set (v) = duelState <- v
  
  type ConnectionDto = {
    Tcp : bool;
    Udp : bool;
  }
  
  type PopupDto = string
  
  type LoginPopupStateDto () =
    let mutable name = null
    
    member m.Name
      with get () = name
      and set (v) = name <- v
      
  type PopupStatesDto () =
    let mutable loginPopupState = Unchecked.defaultof<LoginPopupStateDto>
    
    member m.LoginPopupState
      with get () = loginPopupState
      and set (v) = loginPopupState <- v
      
  type UiComponentDto () =
    [<UnityEngine.SerializeField>]
    let mutable visible = true;
    [<UnityEngine.SerializeField>]
    let mutable interactable = false;
    
    member m.Visible
      with get () = visible
      and set (v) = visible <- v
    member m.Interactable
      with get () = interactable
      and set (v) = interactable <- v
  
  type UiComponentsDto = IDictionary<string, UiComponentDto>
  
  type UiWellDto () =
    let mutable popups = Array.empty;
    let mutable popupStates = Unchecked.defaultof<PopupStatesDto>
    let mutable components = Unchecked.defaultof<UiComponentsDto>
        
    member m.Popups
      with get () = popups
      and set (v) = popups <- v
    member m.PopupStates
      with get () = popupStates
      and set (v) = popupStates <- v
    member m.Components
      with get () = components
      and set (v) = components <- v
    
  type TileWellDto = IDictionary<CoordinateDto, TileDto>
  
  type TileSelectionWellDto () =
    let mutable black = Unchecked.defaultof<SelectionDto>
    let mutable white = Unchecked.defaultof<SelectionDto>

    member m.Black
      with get () = black
      and set (v) = black <- v
    member m.White
      with get () = white
      and set (v) = white <- v
      
  type PieceWellDto = IDictionary<CoordinateDto, PieceDto>
  
  type RuleWellDto = IDictionary<int, RuleDto>
    
  type LifeWellDto (player, duel, connection) =
    let mutable player = player
    let mutable duel = duel
    let mutable connection = connection
  
    new () =
      LifeWellDto (
        Unchecked.defaultof<PlayerDto>,
        Unchecked.defaultof<DuelDto>,
        Unchecked.defaultof<ConnectionDto>
      )
    member m.Player
      with get () = player
      and set (v) = player <- v
    member m.Duel
      with get () = duel
      and set (v) = duel <- v
    member m.Connection
      with get () = connection
      and set (v) = connection <- v
      
  type WellCollectionDto () =
    let mutable lifeWell = Unchecked.defaultof<LifeWellDto>
    let mutable ruleWell = Unchecked.defaultof<RuleWellDto>
    let mutable pieceWell = Unchecked.defaultof<PieceWellDto>
    let mutable tileSelectionWell = Unchecked.defaultof<TileSelectionWellDto>
    let mutable tileWell = Unchecked.defaultof<TileWellDto>
    let mutable uiWell = Unchecked.defaultof<UiWellDto>
    
    member m.LifeWell
      with get () = lifeWell
      and set (v) = lifeWell <- v
    member m.RuleWell
      with get () = ruleWell
      and set (v) = ruleWell <- v
    member m.PieceWell
      with get () = pieceWell
      and set (v) = pieceWell <- v    
    member m.TileSelectionWell
      with get () = tileSelectionWell
      and set (v) = tileSelectionWell <- v
    member m.TileWell
      with get () = tileWell
      and set (v) = tileWell <- v
    member m.UiWell
      with get () = uiWell
      and set (v) = uiWell <- v  
      
module JsonConversions =
  open Types
  open DtoTypes
  open Microsoft.FSharp.Reflection
  open System.Collections.Generic
  open Newtonsoft.Json
  open Well
  open Result
  open Col
  
  let export<'t> (dto : 't) =
    let settings = new JsonSerializerSettings ()
    settings.NullValueHandling <- NullValueHandling.Ignore
    JsonConvert.SerializeObject (dto, settings)
    
  let import<'t> json : Result<'t, string> =
    JsonConvert.DeserializeObject<'t> json
    |> Ok
    
  let private importNullable<'N, 'T when 'N : equality> (n : 'N) importer : Result<Option<'T>, string> =
    n
    |> Nullable.toOption
    |> Option.map importer
    |> Result.pushOutward

  module ColorDto =
    let export color =
      match color with
      | White -> "White"
      | Black -> "Black"
      
    let import color =
      match color with
      | "White" -> Ok White
      | "Black" -> Ok Black
      | _ -> Error ("No such color: " + color) 
    
  module RowDto =
    let export row =
      match row with
      | One -> 1
      | Two -> 2
      | Three -> 3
      | Four -> 4
      | Five -> 5
      | Six -> 6
      | Seven -> 7
      | Eight -> 8
      | Nine -> 9
      | Ten -> 10
      | Eleven -> 11
      | Twelve -> 12
      
    let import row =
      match row with
      | 1 -> Ok One
      | 2 -> Ok Two
      | 3 -> Ok Three
      | 4 -> Ok Four
      | 5 -> Ok Five
      | 6 -> Ok Six
      | 7 -> Ok Seven
      | 8 -> Ok Eight
      | 9 -> Ok Nine
      | 10 -> Ok Ten
      | 11 -> Ok Eleven
      | 12 -> Ok Twelve
      | _ -> Error ("No such row: " + row.ToString())
 
  module ColumnDto =
    let export column =
      match column with
      | A -> "A"
      | B -> "B"
      | C -> "C"
      | D -> "D"
      | E -> "E"
      | F -> "F"
      | G -> "G"
      | H -> "H"
      | I -> "I"
      | J -> "J"
      | K -> "K"
      | L -> "L"
      
    let import column =
      match column with
      | "A" -> Ok A
      | "B" -> Ok B
      | "C" -> Ok C
      | "D" -> Ok D
      | "E" -> Ok E
      | "F" -> Ok F
      | "G" -> Ok G
      | "H" -> Ok H
      | "I" -> Ok I
      | "J" -> Ok J
      | "K" -> Ok K
      | "L" -> Ok L
      | _ -> Error ("No such column: " + column.ToString())
 
  module CoordinateDto =
    let export (row, column) =
      RowDto.export row
      |> string
      |> (fun r -> r + ":" + (ColumnDto.export column))
      
    let import2 row column =
      Ok (fun row column -> (row, column))
      <*> RowDto.import row
      <*> ColumnDto.import column
            
    let import (coordinate : CoordinateDto) =
      coordinate.Split ':'
      |> function
      | [|row; column|] -> import2 (int row) column
      | _ -> Error ("Invalid coordinate: " + coordinate)

  module DuelistTypeDto =
    let export player =
      let dto = new DuelistTypeDto ()
      match player with
      | Any -> dto.Type <- "Any"
      | Self -> dto.Type <- "Self"
      | DuelistType.Other -> dto.Type <- "Other"
      | Player p ->
        dto.Type <- "Player"
        dto.Player <- ColorDto.export p
      dto
      
    let import (player : DuelistTypeDto) =
      match player.Type with
      | "Any" -> Ok Any
      | "Self" -> Ok Self
      | "Other" -> Ok DuelistType.Other
      | "Player" -> ColorDto.import player.Player <!> Player
      | x -> Error ("Invalid duelist type: " + x.ToString())

  module ConditionDto =
    let export condition =
      let dto = new ConditionDto ()
      dto.Type <- Types.Condition.toString condition
      match condition with
      | OccupiedBy p ->
        dto.OccupiedBy <- DuelistTypeDto.export p
        dto
      | OtherPieceType t ->
        dto.OtherPieceType <- t
        dto
      | OtherOwner p ->
        dto.OtherOwner <- DuelistTypeDto.export p
        dto
      | Condition.Row r ->
        dto.Row <- Row.toInt r
        dto
      | Condition.Column c ->
        dto.Column <- Column.toInt c
        dto
      | _ -> dto
      
    let import (condition : ConditionDto) =
      match condition.Type with
      | "Always" -> Always |> Ok
      | "MoveCount" -> MoveCount |> Ok
      | "ExposesKing" -> ExposesKing |> Ok
      | "PathBlocked" -> PathBlocked |> Ok
      | "OccupiedBy" -> condition.OccupiedBy |> DuelistTypeDto.import <!> OccupiedBy
      | "Conquerable" -> Conquerable |> Ok
      | "Movable" -> Movable |> Ok
      | "Defendable" -> Defendable |> Ok
      | "OtherPieceType" -> Nullable.toResult condition.OtherPieceType <!> OtherPieceType
      | "OtherOwner" -> condition.OtherOwner |> DuelistTypeDto.import <!> Condition.OtherOwner
      | "ExposedWhileMoving" -> ExposedWhileMoving |> Ok
      | "Row" -> Row.fromInt condition.Row <!> Condition.Row
      | "Column" -> Column.fromInt condition.Column <!> Condition.Column
      | _ -> Error ("No such condition: " + condition.Type)
    
  module OperatorDto =
    let export operator =
      let dto = new OperatorDto ()
      match operator with
      | Is ->
        dto.Type <- "Is"
      | Not ->
        dto.Type <- "Not"
      | Equals v ->
        dto.Type <- "Equals"
        dto.Value <- v
      | GreaterThan v ->
        dto.Type <- "GreaterThan"
        dto.Value <- v
      | SmallerThan v ->
        dto.Type <- "SmallerThan"
        dto.Value <- v
      dto
    
    let import (operator : OperatorDto) =
      match operator.Type with
      | "Is" -> Ok Is
      | "Not" -> Ok Not
      | "Equals" -> Ok (Equals operator.Value)
      | "GreaterThan" -> Ok (GreaterThan operator.Value)
      | "SmallerThan" -> Ok (SmallerThan operator.Value)
      | t -> Error ("Operator type not found " + t)    

  module ClauseDto =
    let export (operator, condition) =
      let dto = new ClauseDto ()
      dto.Operator <- OperatorDto.export operator
      dto.Condition <- ConditionDto.export condition
      dto
    
    let import (clause : ClauseDto) =
      Ok Tuple.retn2
      <*> OperatorDto.import clause.Operator
      <*> ConditionDto.import clause.Condition

  module ConditionsDto =
    let rec export conditions =
      let dto = new ConditionsDto ()
      match conditions with
      | Clause c ->
        dto.Type <- "Clause"
        dto.Clause <- ClauseDto.export c
      | AllOf c ->
        dto.Type <- "AllOf"
        dto.Clauses <- List.map ClauseDto.export c |> List.toArray
      | OneOf c ->
        dto.Type <- "OneOf"
        dto.Clauses <- List.map ClauseDto.export c |> List.toArray
      | Combination c ->
        dto.Type <- "Combination"
        dto.Combination <- List.map export c |> List.toArray
      dto
      
    let rec import (conditions : ConditionsDto) =
      match conditions.Type with
      | "Clause" ->
        ClauseDto.import conditions.Clause <!> Clause
      | "AllOf" ->
        conditions.Clauses
        |> List.ofArray
        |> List.map ClauseDto.import
        |> Result.unwrap
        <!> AllOf
      | "OneOf" ->
        conditions.Clauses
        |> List.ofArray
        |> List.map ClauseDto.import
        |> Result.unwrap
        <!> OneOf
      | "Combination" ->
        conditions.Combination
        |> List.ofArray
        |> List.map import
        |> Result.unwrap
        <!> Combination
      | t ->
        Error ("No such conditions " + t)
        
  module RuleDto =
    let export rule =
      let dto = new RuleDto ()
      match rule with
      | MoveRule r ->
        dto.Type <- "Move"
        dto.Condition <- ConditionsDto.export r.Condition
        dto.Offset <- Tuple.toArray2 r.Offset
      | ConquerRule r ->
        dto.Type <- "Conquer"
        dto.Condition <- ConditionsDto.export r.Condition
        dto.Offset <- Tuple.toArray2 r.Offset
      | MoveComboRule r ->
        dto.Type <- "MoveCombo"
        dto.Condition <- ConditionsDto.export r.Condition
        dto.Other <- Tuple.toArray2 r.Other
        dto.MyMovement <- Tuple.toArray2 r.MyOffset
        dto.OtherMovement <- Tuple.toArray2 r.OtherOffset
      | PromoteRule r ->
        dto.Type <- "Promote"
        dto.Condition <- ConditionsDto.export r.Condition
        dto.Ranks <- r.Ranks
      | DefeatRule r ->
        dto.Type <- "Defeat"
        dto.Condition <- ConditionsDto.export r.Condition
      | RemiseRule r ->
        dto.Type <- "Remise"
        dto.Condition <- ConditionsDto.export r.Condition
      dto
        
    let import (rule : RuleDto) =
      match rule.Type with
      | "Move" ->
        Ok (fun c o ->
          MoveRule {
            Condition = c;
            Offset = o;
          })
        <*> ConditionsDto.import rule.Condition
        <*> Tuple.fromArray2 rule.Offset
      | "Conquer" ->
        Ok (fun c o ->
          ConquerRule {
            Condition = c;
            Offset = o;
          })
        <*> ConditionsDto.import rule.Condition
        <*> Tuple.fromArray2 rule.Offset
      | "MoveCombo" ->
        Ok (fun c other myOffset otherOffset ->
          MoveComboRule {
            Condition = c;
            Other = other;
            MyOffset = myOffset;
            OtherOffset = otherOffset;
          })
        <*> ConditionsDto.import rule.Condition
        <*> Tuple.fromArray2 rule.Other
        <*> Tuple.fromArray2 rule.MyMovement
        <*> Tuple.fromArray2 rule.OtherMovement
      | "Promote" ->
        Ok (fun c r ->
          PromoteRule {
            Condition = c;
            Ranks = r;
          })
        <*> ConditionsDto.import rule.Condition
        <*> Nullable.toResult rule.Ranks
      | "Defeat" ->
        Ok (fun c ->
          DefeatRule {
            Condition = c;
          })
        <*> ConditionsDto.import rule.Condition
      | "Remise" ->
        Ok (fun c ->
          RemiseRule {
            Condition = c;
          })
        <*> ConditionsDto.import rule.Condition
      | x -> Error ("No such rule: " + x)
      
  module RulesDto =
    let export rules =
      Map.map (fun _ rule -> RuleDto.export rule) rules
      
    let import (rules : IDictionary<int, RuleDto>) =
      Dict rules
      |> Col.map (fun (k, v) -> (k, RuleDto.import v))
      |> Result.unwrapCol
      <!> Col.toMap
        
  module PieceDto =
    let private exportPiece<'t> pieceType (piece : Piece) =
      let pieceDto = new PieceDto ()
      pieceDto.Id <- piece.ID
      pieceDto.Color <- ColorDto.export piece.Color
      pieceDto.Type <- pieceType
      pieceDto.Rules <- List.toArray piece.Rules
      pieceDto.MoveCount <- piece.MoveCount
      pieceDto
  
    let export piece =
      match piece with
      | King king -> exportPiece "King" king
      | Queen queen -> exportPiece "Queen" queen
      | Rook rook -> exportPiece "Rook" rook
      | Bishop bishop -> exportPiece "Bishop" bishop
      | Knight knight -> exportPiece "Knight" knight
      | Pawn pawn -> exportPiece "Pawn" pawn
      
    let inline private importPiece (piece : PieceDto) =
      Ok Piece.create
      <*> Ok piece.Id
      <*> ColorDto.import piece.Color
      <*> Ok (List.ofArray piece.Rules)
      <*> (piece.Coordinate |> Nullable.toOption |> Option.map CoordinateDto.import |> Result.pushOutward)
      <*> Ok piece.MoveCount
      
    let import (piece : PieceDto) =
      match piece.Type with
      | "King" -> importPiece piece <!> King
      | "Queen" -> importPiece piece <!> Queen
      | "Rook" -> importPiece piece <!> Rook
      | "Bishop" -> importPiece piece <!> Bishop
      | "Knight" -> importPiece piece <!> Knight
      | "Pawn" -> importPiece piece <!> Pawn
      | _ -> Error ("No such piece: " + piece.Type)

  module TileDto =
    let export (tile : Tile) =
      let dto = new TileDto ()
      dto.Color <- ColorDto.export tile.Color
      dto
      
    let import (tile : TileDto) : Result<Tile, string> =
      Ok Tile.create
      <*> ColorDto.import tile.Color

  module SelectionDto =
    let export (selection : Selection) : SelectionDto =
      let dto = new SelectionDto ()
      Option.map CoordinateDto.export selection.Selected 
      |> Option.map (fun selected -> dto.Selected <- selected)
      |> ignore
      dto.Conquerable <- List.map CoordinateDto.export selection.Conquerable |> List.toArray
      dto
      
    let import (selection : SelectionDto) =
      Ok Selection.create
      <*> (Nullable.toOption selection.Selected
        |> Option.map CoordinateDto.import
        |> Result.pushOutward)
      <*> (List.ofArray selection.Conquerable
        |> List.map CoordinateDto.import
        |> Result.unwrap)
      
  module PlayerDto =
    let export (player : Player) : PlayerDto =
      {
        Name = player.Name;
      }
      
    let import (player : PlayerDto) : Result<Player, string> =
      match player.Name with
      | null -> Error "No player name provided"
      | name -> Ok name
      <!> Player.create
      
  module DuelistDto =
    let export (duelist : Duelist) : DuelistDto =
      {
        Color = ColorDto.export duelist.Color;
        Name = duelist.Name;
      }
      
    let import (duelist : DuelistDto) =
      Ok Duelist.create
      <*> Ok duelist.Name
      <*> ColorDto.import duelist.Color
      
  module TerritoryDto =
    let export (territory : Territory) =
      match territory with
      | Classic -> "Classic"
      
    let import (territory : string) =
      match territory with
      | "Classic" -> Ok Classic
      | _ -> Error ("No such territory: " + territory)

  module EndedStateDto =
    let export (endedState : EndedState) =
      let dto = new EndedStateDto ()
      match endedState with
      | Remise ->
        dto.Type <- "Remise"
        dto
      | Win color ->
        dto.Type <- "Win"
        dto.Value <- ColorDto.export color
        dto

    let import (endedState : EndedStateDto) =
      match endedState.Type with
      | "Remise" ->
        Ok Remise
      | "Win" -> 
        ColorDto.import endedState.Value
        <!> (fun color -> Win color)
      | t ->
        Error ("No such EndedState: " + t)

  module DuelStateDto =
    let export (duelState : DuelState) =
      let dto = new DuelStateDto ()
      match duelState with
      | Turn duelist ->
        dto.Type <- "Turn"
        dto.Turn <- DuelistTypeDto.export duelist
        dto
      | Paused ->
        dto.Type <- "Paused"
        dto
      | Ended endedState ->
        dto.Type <- "Ended"
        dto.Ended <- EndedStateDto.export endedState
        dto
        
    let import (duelState : DuelStateDto) =
      match duelState.Type with
      | "Turn" ->
        DuelistTypeDto.import duelState.Turn
        <!> (fun turn -> Turn turn)
      | "Paused" ->
        Ok Paused
      | "Ended" ->
        EndedStateDto.import duelState.Ended
        <!> (fun ended -> Ended ended)
      | t ->
        Error ("Failed to import DuelState: " + t)
      
  module DuelDto =
    let export (duel : Duel) =
      let dto = new DuelDto ()
      dto.Duelists <- List.map DuelistDto.export duel.Duelists |> List.toArray
      dto.DuelState <- DuelStateDto.export duel.DuelState
      dto
      
    let import (duel : DuelDto) =
      Ok Duel.create
      <*> (List.map DuelistDto.import (List.ofArray duel.Duelists) |> unwrap)
      <*> DuelStateDto.import duel.DuelState
      
  module ConnectionDto =
    let export (connection : Connection) : ConnectionDto =
      {
        Tcp = connection.Tcp;
        Udp = connection.Udp;
      }
      
    let import (connection : ConnectionDto) : Result<Connection, string> =
      Ok {
        Tcp = connection.Tcp;
        Udp = connection.Udp;
      }
      
  module PopupDto =
    let export (popup : Popup) =
      match popup with
      | Login -> "Login"
      | PlayDuel -> "PlayDuel"
      
    let import (popup : PopupDto) =
      match popup with
      | "Login" -> Ok Login
      | "PlayDuel" -> Ok PlayDuel
      | d -> Error ("No such popup: " + d)
      
  module LoginPopupStateDto =
    let export (loginPopupState : LoginPopupState) =
      let dto = new LoginPopupStateDto ()
      dto.Name <- loginPopupState.Name
      dto
      
    let import (loginPopupState : LoginPopupStateDto) =
      Ok LoginPopupState.create
      <*> Nullable.toResult loginPopupState.Name
      
  module PopupStatesDto =
    let export (popupStates : PopupStates) =
      let dto = new PopupStatesDto ()
      dto.LoginPopupState <- LoginPopupStateDto.export popupStates.LoginPopupState
      dto
      
    let import (popupStates : PopupStatesDto) =
      Ok PopupStates.create
      <*> LoginPopupStateDto.import popupStates.LoginPopupState    

  [<System.Serializable>]
  module UiComponentDto =
    let export (uiComponent : UiComponent) =
      let dto = new UiComponentDto ()
      dto.Visible <- uiComponent.Visible
      dto.Interactable <- uiComponent.Interactable
      dto
      
    let import (uiComponent : UiComponentDto) =
      Ok UiComponent.create
      <*> Nullable.toResult uiComponent.Visible
      <*> Nullable.toResult uiComponent.Interactable
   
  module UiComponentsDto =
    let export (uiComponents : UiComponents) =
      uiComponents
      |> Map
      |> Col.map (fun (k, v) -> (k, UiComponentDto.export(v)))
      |> Col.toDict
    
    let import (uiComponents : UiComponentsDto) =
      uiComponents
      |> Dict
      |> Col.map (fun (k, v) -> (k, UiComponentDto.import(v)))
      |> Result.unwrapCol
      <!> Col.toMap
      
  module UiWellDto =
    let export (ui : UiWell) =
      let dto = new UiWellDto ()
      dto.Popups <- List.map PopupDto.export ui.Popups |> List.toArray
      dto.PopupStates <- PopupStatesDto.export ui.PopupStates
      dto.Components <- UiComponentsDto.export ui.Components
      dto
      
    let import (ui : UiWellDto) =
      Ok UiWell.create
      <*> (ui.Popups |> (List.ofArray >> List.map PopupDto.import >> Result.unwrap))
      <*> PopupStatesDto.import ui.PopupStates
      <*> UiComponentsDto.import ui.Components
      
  module TileWellDto =
    let export (tileWell : TileWell) =
      Map tileWell
      |> Col.map (fun (k, v) -> (CoordinateDto.export k, TileDto.export v))
      |> Col.toDict
      
    let import (tileWell : TileWellDto) =
      Dict tileWell
      |> Col.reduce (fun c (k, v) ->
        match CoordinateDto.import k, TileDto.import v, c with
        | Ok k, Ok v, Ok c -> Col.add k v c |> Ok
        | _, _, Error e -> Error e
        | _, Error e, _ -> Error e
        | Error e, _, _ -> Error e
      ) (Map Map.empty |> Ok)
      <!> Col.toMap
   
  module TileSelectionWellDto =
    let export (selections : TileSelectionWell) =
      let dto = new TileSelectionWellDto ()
      dto.Black <- SelectionDto.export selections.Black
      dto.White <- SelectionDto.export selections.White
      dto
      
    let import (selections : TileSelectionWellDto) =
      Ok TileSelectionWell.create
      <*> SelectionDto.import selections.Black
      <*> SelectionDto.import selections.White    
  
  module PieceWellDto =
    let export (pieceWell : PieceWell) =
      Map pieceWell
      |> Col.map (fun (k, v) -> (CoordinateDto.export k, PieceDto.export v))
      |> Col.toDict
      
    let import (pieceWell : PieceWellDto) =
      Dict pieceWell
      |> Col.reduce (fun c (k, v) ->
        match CoordinateDto.import k, PieceDto.import v, c with
        | Ok coord, Ok piece, Ok c ->
          Col.add coord (Types.Pieces.update (fun p -> { p with Coordinate = Some coord }) piece) c |> Ok
        | _, _, Error e -> Error e
        | _, Error e, _ -> Error e
        | Error e, _, _ -> Error e
      ) (Map Map.empty |> Ok)
      <!> Col.toMap
      
  module RuleWellDto =
    let export (ruleWell : RuleWell) =
      Map ruleWell
      |> Col.map (fun (k, v) -> (k, RuleDto.export v))
      |> Col.toDict
      
    let import (ruleWell : RuleWellDto) =
      Dict ruleWell
      |> Col.reduce (fun c (k, v) ->
        match RuleDto.import v, c with
        | Ok piece, Ok c -> Col.add k piece c |> Ok
        | _, Error e -> Error e
        | Error e, _ -> Error e
      ) (Map Map.empty |> Ok)
      <!> Col.toMap  
            
  module LifeWellDto =
    let export (lifewell : LifeWell) =
      let duel =
        match lifewell.Duel with
        | Some duel -> DuelDto.export duel
        | _ -> Unchecked.defaultof<DuelDto>
        
      let player =
        match lifewell.Player with
        | Some player -> PlayerDto.export player
        | _ -> Unchecked.defaultof<PlayerDto>
        
      new LifeWellDto (
        player,
        duel,
        ConnectionDto.export lifewell.Connection
      )
      
    let import (lifewell : LifeWellDto) =
      let duel =
        if Unchecked.defaultof<DuelDto> = lifewell.Duel
        then Ok None
        else DuelDto.import lifewell.Duel <!> Some
      let player =
        if Unchecked.defaultof<PlayerDto> = lifewell.Player
        then Ok None
        else PlayerDto.import lifewell.Player <!> Some

      Ok LifeWell.create
      <*> player
      <*> duel
      <*> ConnectionDto.import lifewell.Connection

  module WellCollectionDto =
    let export (wellCollection : WellCollection) =
      let dto = new WellCollectionDto ()
      Option.map (fun lifeWell -> dto.LifeWell <- LifeWellDto.export lifeWell) wellCollection.LifeWell |> ignore
      Option.map (fun ruleWell -> dto.RuleWell <- RuleWellDto.export ruleWell) wellCollection.RuleWell |> ignore
      Option.map (fun pieceWell -> dto.PieceWell <- PieceWellDto.export pieceWell) wellCollection.PieceWell |> ignore
      Option.map (fun tileSelectionWell -> dto.TileSelectionWell <- TileSelectionWellDto.export tileSelectionWell) wellCollection.TileSelectionWell |> ignore
      Option.map (fun tileWell -> dto.TileWell <- TileWellDto.export tileWell) wellCollection.TileWell |> ignore
      Option.map (fun uiWell -> dto.UiWell <- UiWellDto.export uiWell) wellCollection.UiWell |> ignore
      dto
      
    let import (wellCollection : WellCollectionDto) =
      Ok (fun lifeWell ruleWell pieceWell tileSelectionWell tileWell uiWell ->
        { 
          LifeWell = lifeWell;
          RuleWell = ruleWell;
          PieceWell = pieceWell;
          TileSelectionWell = tileSelectionWell;
          TileWell = tileWell;
          UiWell = uiWell
        }
      )
      <*>
        match Nullable.toOption wellCollection.LifeWell with
        | Some well -> LifeWellDto.import well |> Result.map Some
        | None -> Ok None
      <*>
        match Nullable.toOption wellCollection.RuleWell with
        | Some well -> RuleWellDto.import well |> Result.map Some
        | None -> Ok None
      <*>
        match Nullable.toOption wellCollection.PieceWell with
        | Some well -> PieceWellDto.import well |> Result.map Some
        | None -> Ok None
      <*>
        match Nullable.toOption wellCollection.TileSelectionWell with
        | Some well -> TileSelectionWellDto.import well |> Result.map Some
        | None -> Ok None
      <*>
        match Nullable.toOption wellCollection.TileWell with
        | Some well -> TileWellDto.import well |> Result.map Some
        | None -> Ok None
      <*>
        match Nullable.toOption wellCollection.UiWell with
        | Some well -> UiWellDto.import well |> Result.map Some
        | None -> Ok None
      