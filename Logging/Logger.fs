namespace ChessPlus

module Logger =
  open UnityEngine
  open System
  
  let log = Debug.Log
  
  let warn = Debug.LogWarning
  
  let error = Debug.LogError
  
  let inspect label msg =
    log label
    log msg
    msg
    
  let printTimer<'t> (startTime : System.DateTime) label (state : 't) =
    log (label + " end: " + startTime.Second.ToString() + ":" + startTime.Millisecond.ToString())
    System.DateTime.Now - startTime
    |> (fun ts -> ts.Milliseconds)
    |> inspect label
    |> ignore
    state
          
  let time label =
    let mark = System.DateTime.Now
    log (label + " start: " + mark.Second.ToString() + ":" + mark.Millisecond.ToString())
    printTimer mark label
    
  let now label =
    let mark = System.DateTime.Now
    let min = String.Format ("{0:D2}", mark.Minute)
    let sec = String.Format ("{0:D2}", mark.Second)
    let mili = String.Format ("{0:D3}", mark.Millisecond)
    log (label + ": " + min + ":" + sec + ":" + mili)
  
  let nowP label s =
    now label
    s
  
  // TODO make debug module ?
  let getUnionName (x: 'a) =
    match Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name
  
  let logUnionName (x: 'a) =
    getUnionName x |> log