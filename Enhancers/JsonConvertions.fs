namespace ChessPlus
open System.Collections.Generic

module DtoTypes =  
  type ColorDto = string
     
  type RowDto = int      
  type ColumnDto = string      
  type CoordinateDto = {
    Row : int;
    Column : string;
  }
  
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
    
    member m.Type
      with get () = ``type``
      and set (v) = ``type`` <- v
    member m.OccupiedBy
      with get () = occupiedBy
      and set (v) = occupiedBy <- v
            
  type OperatorDto () =
    let mutable ``type`` = null
    let mutable value = Unchecked.defaultof<int>
    
    member m.Type
      with get () = ``type``
      and set (v) = ``type`` <- v
    member m.Value
      with get () = value
      and set (v) = value <- v
      
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
  
  type PieceDto (pieceType, color) =
    let mutable ``type`` = pieceType
    let mutable color = color
    
    new (pieceType) =
      PieceDto (pieceType, Unchecked.defaultof<ColorDto>)
    new () =
      PieceDto (Unchecked.defaultof<string>, Unchecked.defaultof<ColorDto>)
      
    member m.Type
      with get () = ``type``
      and set (v) = ``type`` <- v
    member m.Color
      with get () = color
      and set (v) = color <- v

  type TileDto (color, piece, selectedBy, conquerableBy) =
    let mutable color = color
    let mutable piece = piece
    let mutable selectedBy = selectedBy
    let mutable conquerableBy = conquerableBy
    
    new (color, piece, selectedBy) =
      TileDto (color, piece, selectedBy, Unchecked.defaultof<ColorDto>)
    new (color, piece) =
      TileDto (color, piece, Unchecked.defaultof<ColorDto>, Unchecked.defaultof<ColorDto>)
    new () =
      TileDto (Unchecked.defaultof<ColorDto>, Unchecked.defaultof<PieceDto>)
      
    member m.Color
      with get () = color
      and set (v) = color <- v
    member m.Piece
      with get () = piece
      and set (v) = piece <- v
    member m.SelectedBy
      with get () = selectedBy
      and set (v) = selectedBy <- v 
    member m.ConquerableBy
      with get () = conquerableBy
      and set (v) = conquerableBy <- v
      
  type BoardDto = {
    Tiles : IDictionary<int, IDictionary<string, TileDto>>;
  }
  type PlayerDto = {
    Name : string;
  }
  type DuelistDto = {
    Name : string;
    Color : ColorDto;
  }
  type TerritoryDto = string
  type DuelDto = {
    Duelists : DuelistDto array;
    Board : BoardDto;
    Rules : RulesDto;
  }
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
  
  type UiDto () =
    let mutable popups = Array.empty;
    let mutable popupStates = Unchecked.defaultof<PopupStatesDto>
    
    member m.Popups
      with get () = popups
      and set (v) = popups <- v
    member m.PopupStates
      with get () = popupStates
      and set (v) = popupStates <- v
    
  type LifeWellDto (player, duel, connection, ui) =
    let mutable player = player
    let mutable duel = duel
    let mutable connection = connection
    let mutable ui = ui
  
    new () =
      LifeWellDto (
        Unchecked.defaultof<PlayerDto>,
        Unchecked.defaultof<DuelDto>,
        Unchecked.defaultof<ConnectionDto>,
        Unchecked.defaultof<UiDto>
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
    member m.Ui
      with get () = ui
      and set (v) = ui <- v

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
      | _ -> Error ("No such column: " + column.ToString())
 
  module CoordinateDto =
    let export (row, column) =
      {
        Row = RowDto.export row;
        Column = ColumnDto.export column
      }
      
    let import coordinate =
      Ok (fun row column -> (row, column))
      <*> RowDto.import coordinate.Row
      <*> ColumnDto.import coordinate.Column
      
    let import2 row column =
      Ok (fun row column -> (row, column))
      <*> RowDto.import row
      <*> ColumnDto.import column

  module DuelistTypeDto =
    let export player =
      let dto = new DuelistTypeDto ()
      match player with
      | Any -> dto.Type <- "Any"
      | Self -> dto.Type <- "Self"
      | Other -> dto.Type <- "Other"
      | Player p ->
        dto.Type <- "Player"
        dto.Player <- ColorDto.export p
      dto
      
    let import (player : DuelistTypeDto) =
      match player.Type with
      | "Any" -> Ok Any
      | "Self" -> Ok Self
      | "Other" -> Ok Other
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
      let pieceDto = new PieceDto(pieceType, ColorDto.export piece.Color)
      pieceDto
  
    let export piece =
      match piece with
      | Some (King king) -> exportPiece "King" king
      | Some (Queen queen) -> exportPiece "Queen" queen
      | Some (Rook rook) -> exportPiece "Rook" rook
      | Some (Bishop bishop) -> exportPiece "Bishop" bishop
      | Some (Knight knight) -> exportPiece "Knight" knight
      | Some (Pawn pawn) -> exportPiece "Pawn" pawn
      | _ -> new PieceDto()
      
    let inline private importPiece (piece : PieceDto) =
      Ok Piece.create
      <*> ColorDto.import piece.Color
      
    let import (piece : PieceDto) =
      match piece.Type with
      | "King" -> importPiece piece <!> King <!> Some
      | "Queen" -> importPiece piece <!> Queen <!> Some
      | "Rook" -> importPiece piece <!> Rook <!> Some
      | "Bishop" -> importPiece piece <!> Bishop <!> Some
      | "Knight" -> importPiece piece <!> Knight <!> Some
      | "Pawn" -> importPiece piece <!> Pawn <!> Some
      | _ -> Error ("No such piece: " + piece.Type)

  module TileDto =
    let export (tile : Tile) =
      new TileDto (
        ColorDto.export tile.Color,
        PieceDto.export tile.Piece,
        tile.SelectedBy |> Nullable.fromOption |> Nullable.map ColorDto.export,
        tile.ConquerableBy |> Nullable.fromOption |> Nullable.map ColorDto.export
      )
    let import (tile : TileDto) : Result<Tile, string> =
      let piece =
        match Nullable.toOption tile.Piece with
        | None -> Ok None
        | Some pieceDto -> PieceDto.import pieceDto
        
      Ok Tile.create
      <*> ColorDto.import tile.Color
      <*> piece
      <*> (importNullable tile.SelectedBy ColorDto.import)
      <*> (importNullable tile.ConquerableBy ColorDto.import)

  module BoardDto =
    let export (board : Board) : BoardDto =
      let makeTile (k, v) = (ColumnDto.export k, TileDto.export v)
      let makeTiles (k, v) = (RowDto.export k, Map v
        |> Col.map makeTile
        |> Col.toDict
      )
      let tiles = 
        Map board.Tiles
        |> Col.map makeTiles
        |> Col.toDict
      {
        Tiles = tiles;
      }
    
    let import (board : BoardDto) : Result<Board, string> =
      Matrix.fromDict board.Tiles
      |> Matrix.fold (fun s r c v ->
        Ok Matrix.add
        <*> RowDto.import r
        <*> ColumnDto.import c
        <*> TileDto.import v
        <*> s
      ) (Ok Matrix.empty)
      <!> fun tiles -> ({ Tiles = tiles } : Board)
      
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
      
  module DuelDto =
    let export (duel : Duel) =
      let duelists = List.map DuelistDto.export duel.Duelists |> List.toArray;
      {
        Duelists = duelists;
        Board = BoardDto.export duel.Board;
        Rules = RulesDto.export duel.Rules;
      }
      
    let import (duel : DuelDto) =
      Ok Duel.create
      <*> (List.map DuelistDto.import (List.ofArray duel.Duelists) |> unwrap)
      <*> BoardDto.import duel.Board
      <*> RulesDto.import duel.Rules
      
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
      
  module UiDto =
    let export (ui : Ui) =
      let dto = new UiDto ()
      dto.Popups <- List.map PopupDto.export ui.Popups |> List.toArray
      dto.PopupStates <- PopupStatesDto.export ui.PopupStates
      dto
      
    let import (ui : UiDto) =
      Ok Ui.create
      <*> (ui.Popups |> (List.ofArray >> List.map PopupDto.import >> Result.unwrap))
      <*> PopupStatesDto.import ui.PopupStates
      
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
        ConnectionDto.export lifewell.Connection,
        UiDto.export lifewell.Ui
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
      <*> UiDto.import lifewell.Ui
