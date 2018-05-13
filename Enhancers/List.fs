namespace ChessPlus

module List =
  let contains<'t when 't : equality> (item : 't) (lst : 't list) : bool =
    List.tryFind (fun v -> item = v) lst <> None