namespace ChessPlus
open System.Collections.Generic

module DtoTypes =  
  type ColorDto = string
  type PieceDto (pieceType, color) =
    new (pieceType) =
      PieceDto (pieceType, Unchecked.defaultof<ColorDto>)
    member m.Type = pieceType
    member m.Color = color
  type RowDto = int      
  type ColumnDto = string      
  type CoordinateDto = {
    Row : int;
    Column : string;
  }
  type TileDto (color, piece, selectedBy, conquerableBy) =
    new (color, piece, selectedBy) =
      TileDto (color, piece, selectedBy, Unchecked.defaultof<ColorDto>)
    new (color, piece) =
      TileDto (color, piece, Unchecked.defaultof<ColorDto>, Unchecked.defaultof<ColorDto>)
    member m.Color = color
    member m.Piece = piece
    member m.SelectedBy = selectedBy
    member m.ConquerableBy = conquerableBy
      
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
  type DuelDto = {
    Duelists : list<DuelistDto>;
    Board : BoardDto;
  }
  type LifeWellDto (player, duel) =
    member m.Player = player
    member m.Duel = duel

module JsonConversions =
  open Types
  open DtoTypes
  open Microsoft.FSharp.Reflection
  open System.Collections.Generic
  open Newtonsoft.Json
  open Well
  open Result
  open Col
  
  let export dto =
    let settings = new JsonSerializerSettings ()
    settings.NullValueHandling <- NullValueHandling.Ignore
    JsonConvert.SerializeObject (dto, settings)
    
  let import<'t> json =
    JsonConvert.DeserializeObject<'t> json
    |> Ok
    
  let private importNullable<'T> n importer : Result<Option<'T>, string> =
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
      | _ -> new PieceDto("None")
      
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
      | "None" -> Ok None
      | _ -> Error ("No such piece: " + piece.Type)
    
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

  module TileDto =
    let export (tile : Tile) =
      new TileDto (
        ColorDto.export tile.Color,
        PieceDto.export tile.Piece,
        tile.SelectedBy |> Nullable.fromOption |> Nullable.map ColorDto.export,
        tile.ConquerableBy |> Nullable.fromOption |> Nullable.map ColorDto.export
      )
    let import (tile : TileDto) : Result<Tile, string> =
      Ok Tile.create
      <*> ColorDto.import tile.Color
      <*> PieceDto.import tile.Piece
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
      let makeTileKV (k, v) =
        Ok (fun column tile -> (column, tile))
        <*> ColumnDto.import k
        <*> TileDto.import v
      let tileMerger col tileKV =
        Ok (fun col (k, v) -> Col.add k v col)
        <*> col
        <*> makeTileKV tileKV
      let makeRowKV (k, v) =
        Ok (fun row tiles -> (row, (Col.toMap tiles)))
        <*> RowDto.import k
        <*> (Dict v |> Col.reduce tileMerger (Map.empty |> Map |> Ok))
      let rowMerger col rowKV =
        Ok (fun col (k, v) -> Col.add k v col)
        <*> col
        <*> makeRowKV rowKV
      let tiles =
        Dict board.Tiles
        |> Col.reduce rowMerger (Map.empty |> Map |> Ok)
      Ok (fun (tiles : Col<Row, Map<Column, Tile>>) -> Board.create (Col.toMap tiles))
      <*> tiles
      
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
      
  module DuelDto =
    let export (duel : Duel) =
      let duelists = List.map DuelistDto.export duel.Duelists;
      {
        Duelists = duelists;
        Board = BoardDto.export duel.Board;
      }
      
    let import (duel : DuelDto) =
      Ok Duel.create
      <*> (List.map DuelistDto.import duel.Duelists |> unwrap)
      <*> BoardDto.import duel.Board
      
  module LifeWellDto =
    let export (lifewell : LifeWell) =
      let duel =
        match lifewell.Duel with
        | Some duel -> DuelDto.export duel
        | _ -> Unchecked.defaultof<DuelDto>
      new LifeWellDto (PlayerDto.export lifewell.Player, duel)
      
    let import (lifewell : LifeWellDto) =
      let duel =
        if Unchecked.defaultof<DuelDto> = lifewell.Duel
        then Ok None
        else DuelDto.import lifewell.Duel <!> Some

      Ok LifeWell.create
      <*> PlayerDto.import lifewell.Player
      <*> duel