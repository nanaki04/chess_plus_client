namespace ChessPlus

module Logger =
  open UnityEngine
  
  let log = Debug.Log
  
  let warn = Debug.LogWarning
  
  let error = Debug.LogError
  
  let inspect label msg =
    log label
    log msg
    msg
    
  let printTimer<'t> (startTime : System.DateTime) label (state : 't) =
    System.DateTime.Now - startTime
    |> (fun ts -> ts.Milliseconds)
    |> inspect label
    |> ignore
    state
          
  let time label =
    let mark = System.DateTime.Now
    printTimer mark label
  
  // TODO make debug module ?
  let getUnionName (x: 'a) =
    match Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name
  