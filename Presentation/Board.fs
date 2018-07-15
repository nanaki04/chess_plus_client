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
  
  member private m.MakeTile row column t =
    tile |> Nullable.toOption |> Option.map (fun (obj : GameObject) ->
      GameObject.Instantiate (obj, m.transform) :?> GameObject
      |> (fun obj -> obj.GetComponent<TileView>())
      |> (fun tileView -> tileView.Init row column t)
      |> (fun tileView -> ((row, column), tileView))
    )
    
  member private m.RemoveTiles (tiles, highlightedTiles, pieces) =
    Map.iter (fun _ (t : TileView) -> GameObject.Destroy(t.gameObject)) tiles
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
    List.iter (fun (t : TileView) -> t.ResetColor () |> ignore) highlightedTiles
    (tiles, List.empty, pieces)
    
  member private m.HighlightTiles tileSelectionWell (tiles : Map<Coordinate, TileView>, highlightedTiles, pieces) =
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

  member private m.SpawnPieces pieceWell (tiles : Map<Coordinate, TileView>, highlightedTiles, pieces) =
    pieceWell
    |> Map.fold (fun (newPieces, oldPieces) coord piece ->
      let p = Types.Pieces.map id piece
      match Map.tryFind p.ID pieces with
      | Some (pieceView : PieceView) -> pieceView.Set piece
      | None -> pieceFactory.Spawn piece coord
      |> (fun pieceView ->
        Map.tryFind coord tiles
        <!> (fun tileView -> tileView.AddPiece pieceView)
        |> ignore
        pieceView
      )
      |> (fun pieceView -> Map.add p.ID pieceView newPieces)
      |> Tuple.add1 (Map.remove p.ID oldPieces)
    ) (Map.empty, pieces)
    |> (fun (newPieces, oldPieces) ->
      Map.iter (fun _ (p : PieceView) -> p.Remove ()) oldPieces
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