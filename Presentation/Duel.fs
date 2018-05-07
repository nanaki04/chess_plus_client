namespace ChessPlus

open UnityEngine
open UnityEngine.UI
open Observers
open Well
open Result

type DuelView () =
  inherit Presentation ()

  [<SerializeField>]
  let mutable (boardPrefab : GameObject) = null
  let mutable (board : Option<BoardView>) = None      
  let mutable unsubscribe = fun () -> ()
    
  member m.OnDuelChange duel _ =
    match duel, board, Nullable.toOption boardPrefab with
    | Some d, None, Some p ->
      board <- (GameObject.Instantiate (p, m.transform) :?> GameObject).GetComponent<BoardView>()
      |> (fun b -> b.Init d.Board)
      |> Some
    | None, Some b, _ ->
      GameObject.Destroy(b)
    | _, _, _ ->
      ()
    
  override m.Start () =
    base.Start ()
    unsubscribe <- observeDuel m.OnDuelChange
    
  override m.OnDestroy () =
    base.OnDestroy ()
    unsubscribe ()
    
    Option.map (fun b -> GameObject.Destroy(b)) board
    |> ignore