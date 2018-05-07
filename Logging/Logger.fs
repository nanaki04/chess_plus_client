namespace ChessPlus

module Logger =
  open UnityEngine
  
  let log = Debug.Log
  
  let warn = Debug.LogWarning
  
  let error = Debug.LogError
  
  // TODO make debug module ?
  let getUnionName (x: 'a) =
    match Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name
  