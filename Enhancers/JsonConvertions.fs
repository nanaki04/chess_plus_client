namespace ChessPlus
open System.Collections.Generic

module DtoTypes =  
  type ColorDto = string
  type RowDto = int      
  type ColumnDto = string      
  type CoordinateDto = {
    Row : int;
    Column : string
  }
  type TileDto = {
    Color : ColorDto
  }
  type BoardDto = {
    Tiles : IDictionary<int, IDictionary<string, TileDto>>
  }    
  type LifeWellDto = {
    Board : BoardDto;
  }

module JsonConversions =
  open DtoTypes
  open Microsoft.FSharp.Reflection
  open System.Collections.Generic
  open Newtonsoft.Json
  open Well
  open Result
  open Col
  
  let export dto =
    dto
    |> JsonConvert.SerializeObject
    
  let import<'t> json =
    JsonConvert.DeserializeObject<'t> json
    |> Ok
    
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

  module TileDto =
    let export (tile : Tile) =
      {
        Color = ColorDto.export tile.Color
      }
      
    let import (tile : TileDto) : Result<Tile, string> =
      Ok Tile.create
      <*> ColorDto.import tile.Color

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
        <*> (makeRowKV rowKV)
      let tiles =
        Dict board.Tiles
        |> Col.reduce rowMerger (Map.empty |> Map |> Ok)
      Ok (fun (tiles : Col<Row, Map<Column, Tile>>) -> Board.create (Col.toMap tiles))
      <*> tiles
    
  module LifeWellDto =
    let export (lifewell : LifeWell) =
      {
        Board = (BoardDto.export lifewell.Board)
      }
      
    let import lifeWell =
      Ok LifeWell.create
      <*> (BoardDto.import lifeWell.Board)