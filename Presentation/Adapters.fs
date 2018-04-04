namespace ChessPlus

type ColorEnumDto =
| Black = 0
| White = 1

module Adapters =
  open UnityEngine.UI
  open System
 
  module ColorEnumDto =
    let export color =
      match color with
      | White -> ColorEnumDto.White
      | Black -> ColorEnumDto.Black
      
    let import color =
      match color with
      | ColorEnumDto.White -> Ok White
      | ColorEnumDto.Black -> Ok Black
      | _ -> Error ("No such color: " + color.ToString())

  module Nullable =
    let maybe f v =
      match v with
      | null -> ()
      | v -> f v
