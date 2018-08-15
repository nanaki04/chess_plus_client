namespace ChessPlus

open UnityEngine
open UnityEngine.UI
open Observers
open Well
open Result

type BoardView () =
  inherit Presentation ()

  [<SerializeField>]
  let mutable (tile : GameObject) = null
  
  [<SerializeField>]
  let mutable pieceFactory = Unchecked.defaultof<PieceFactoryView>
  
  let mutable unsubscribeTiles = fun () -> ()
  let mutable unsubscribeTileSelections = fun () -> ()
  let mutable unsubscribePieces = fun () -> ()
  
  let call = Agent.start (fun () -> (Map.empty, List.empty, Map.empty))
  
  let (>>=) = fun v f -> Option.bind f v
  let (<!>) = fun f v -> Option.map v f
  let (<*>) = Option.apply
  
  member private m.InstantiateTile () =
    tile
    |> Nullable.toOption
    |> Option.map (fun prefab -> GameObject.Instantiate prefab :?> GameObject)
    |> Option.bind (fun t ->
      match Nullable.toOption (t.GetComponent<TileView> ()), Nullable.toOption (t.GetComponent<Tile3DView> ()) with
      | Some tileView, _ -> tileView :> ITileView |> Some
      | _, Some tile3DView -> tile3DView :> ITileView |> Some
      | _ -> None
    )
  
  member private m.MakeTile row column t =
    m.InstantiateTile ()
    |> Option.map (fun tileView -> tileView.Init row column t)
    |> Option.map (fun tileView -> ((row, column), tileView))
    
  member private m.RemoveTiles (tiles, highlightedTiles, pieces) =
    Map.iter (fun _ (t : ITileView) -> t.Destroy ()) tiles
    (Map.empty, highlightedTiles, pieces)
    
  member private m.MakeTiles tileWell (tiles, highlightedTiles, pieces) =
    Map.fold (fun tiles (r, c) v ->
      m.MakeTile r c v
      |> Option.map (fun t -> t::tiles)
      |> Option.defaultValue tiles
    ) List.empty tileWell
    |> Map.ofList
    |> fun newTiles -> (newTiles, highlightedTiles, pieces)
    
  member private m.ResetTileColors (tiles, highlightedTiles, pieces) =
    List.iter (fun (t : ITileView) -> t.ResetColor () |> ignore) highlightedTiles
    (tiles, List.empty, pieces)
    
  member private m.HighlightTiles tileSelectionWell (tiles : Map<Coordinate, ITileView>, highlightedTiles, pieces) =
    [
      tileSelectionWell.White.Selected
      >>= (fun s -> Map.tryFind s tiles)
      <!> (fun t -> [t.HighlightAsSelected White]);
      
      tileSelectionWell.Black.Selected
      >>= (fun s -> Map.tryFind s tiles)
      <!> (fun t -> [t.HighlightAsSelected Black]);
      
      List.map (fun c ->
        Map.tryFind c tiles
        <!> (fun t -> t.HighlightAsConquerable White)
      ) tileSelectionWell.White.Conquerable
      |> Option.unwrap;
      
      List.map (fun c ->
        Map.tryFind c tiles
        <!> (fun t -> t.HighlightAsConquerable Black)
      ) tileSelectionWell.Black.Conquerable
      |> Option.unwrap;
    ]
    |> List.fold (fun acc tiles -> Option.map (List.append acc) tiles |> Option.defaultValue acc) []
    |> fun newHighlightedTiles -> (tiles, newHighlightedTiles, pieces)   

  member private m.SpawnPieces pieceWell (tiles : Map<Coordinate, ITileView>, highlightedTiles, pieces) =
    pieceWell
    |> Map.fold (fun (newPieces, oldPieces) coord piece ->
      let p = Types.Pieces.map id piece
      let pieceView =
        match Map.tryFind p.ID pieces with
        | Some (pieceView : IPieceView) ->
          if pieceView.IsPieceType piece then
            pieceView.Set piece |> Some
          else
            pieceView.Remove ()
            pieceFactory.Spawn piece coord
        | None ->
          pieceFactory.Spawn piece coord
          
      pieceView
      |> Option.map (fun pieceView ->
        Map.tryFind coord tiles
        <!> (fun tileView -> tileView.AddPiece pieceView)
        |> ignore
        pieceView
      )
      |> Option.map (fun pieceView -> Map.add p.ID pieceView newPieces)
      |> Option.map (fun newPieces -> Tuple.add1 (Map.remove p.ID oldPieces) newPieces)
      |> Option.defaultValue (newPieces, oldPieces)
    ) (Map.empty, pieces)
    |> (fun (newPieces, oldPieces) ->
      Map.iter (fun _ (p : IPieceView) -> p.Remove ()) oldPieces
      newPieces
    )
    |> fun newPieces -> (tiles, highlightedTiles, newPieces)
      
  member m.OnTilesChanged tileWell _ =
    m.RemoveTiles
    >> m.MakeTiles tileWell
    |> call
    
  member m.OnTileSelectionsChanged tileSelectionWell _ =
    m.ResetTileColors
    >> m.HighlightTiles tileSelectionWell
    |> call
    
  member m.OnPiecesChanged pieceWell _ =
    m.SpawnPieces pieceWell
    |> call
    
  override m.Start () =
    base.Start ()
    unsubscribeTiles <- observeTiles m.OnTilesChanged
    unsubscribeTileSelections <- observeSelections m.OnTileSelectionsChanged
    unsubscribePieces <- observePieces m.OnPiecesChanged
    
  override m.OnDestroy () =
    base.OnDestroy ()
    unsubscribeTiles ()
    unsubscribeTileSelections ()
    unsubscribePieces ()