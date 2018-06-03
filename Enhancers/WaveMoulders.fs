namespace ChessPlus
open JsonConversions
open DtoTypes
open Waves

module Moulds =
  open Result
  open System

  type Importable =
    abstract member import : unit -> Result<Maelstrom.Wave<Amplitude>, string>
  
  type LocationMould () =
    let mutable domain = null
    let mutable invocation = null
    
    member m.Domain
      with get() = domain
      and set(v) = domain <- v
    member m.Invocation
      with get() = invocation
      and set(v) = invocation <- v
      
    static member export ((domain, invocation)) =
      let m = new LocationMould ()
      m.Domain <- domain
      m.Invocation <- invocation
      m
      
    member m.import () =
      Ok (m.Domain, m.Invocation)
        
  type Mould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    
    member m.Location
      with get() = location
      and set(v) = location <- v
    
    member m.import() =
      m.Location.import ()
      
    static member export location =
      let dto = new Mould ()
      dto.Location <- LocationMould.export location
      dto
  
  type AddTileMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable coordinate = Unchecked.defaultof<CoordinateDto>
    let mutable tile = Unchecked.defaultof<TileDto>
    
    member m.Location
      with get() = location
      and set(v) = location <- v
    member m.Coordinate
      with get() = coordinate
      and set(v) = coordinate <- v
    member m.Tile
      with get() = tile
      and set(v) = tile <- v
      
    static member export (wave) =
      let (loc, ampl : AddTileAmplitude) = wave
      let m = new AddTileMould ()
      m.Location <- LocationMould.export loc
      m.Coordinate <- CoordinateDto.export ampl.Coordinate
      m.Tile <- TileDto.export ampl.Tile
      m
    
    interface Importable with
      member m.import () =
        Ok (fun coord tile ->
          AddTileAmplitude {
            Coordinate = coord;
            Tile = tile;
          })
        <*> CoordinateDto.import m.Coordinate
        <*> TileDto.import m.Tile
        <!> addTileWave        
  
  type LoginMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable name = null
    
    member m.Location 
      with get() = location
      and set(v) = location <- v
    member m.Name
      with get() = name
      and set(v) = name <- v
    
    static member export (wave) =
      let (loc, ampl : LoginAmplitude) = wave
      let m = new LoginMould ()
      m.Location <- LocationMould.export loc
      m.Name <- ampl.Name
      m
    
    interface Importable with
      member m.import () =
        Ok (fun name ->
          LoginAmplitude {
            Name = name;
          })
        <*> Nullable.toResult m.Name
        <!> loginWave
        
  type PlayerCreatedMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable player = Unchecked.defaultof<PlayerDto>
    
    member m.Location
      with get() = location
      and set(v) = location <- v
    member m.Player
      with get() = player
      and set(v) = player <- v
    
    static member export (wave) =
      let (loc, ampl : PlayerCreatedAmplitude) = wave
      let m = new PlayerCreatedMould ()
      m.Location <- LocationMould.export loc
      m.Player <- PlayerDto.export ampl.Player
      m
      
    interface Importable with
      member m.import () =
        Ok (fun player ->
          PlayerCreatedAmplitude {
            Player = player;
          })
        <*> PlayerDto.import m.Player
        <!> playerCreatedWave

  type ConfirmLoginMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable player = Unchecked.defaultof<PlayerDto>
    
    member m.Location
      with get() = location
      and set(v) = location <- v
    member m.Player
      with get() = player
      and set(v) = player <- v
      
    static member export(wave) =
      let (loc, ampl : ConfirmLoginAmplitude) = wave
      let m = new ConfirmLoginMould ()
      m.Location <- LocationMould.export loc
      m.Player <- PlayerDto.export ampl.Player
      m
    
    interface Importable with
      member m.import () =
        Ok (fun player ->
          ConfirmLoginAmplitude {
            Player = player;
          })
        <*> PlayerDto.import m.Player
        <!> confirmLoginWave

  type ReportFailureMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable reason = null
    
    member m.Location
      with get() = location
      and set(v) = location <- v    
    member m.Reason
      with get() = reason
      and set(v) = reason <- v  
      
    static member export (wave) =
      let (loc, ampl : ReportFailureAmplitude) = wave
      let m = new ReportFailureMould ()
      m.Location <- LocationMould.export loc
      m.Reason <- ampl.Reason
      m
    
    interface Importable with
      member m.import () = 
        Ok (fun reason ->
          ReportFailureAmplitude {
            Reason = reason;
          })
        <*> Nullable.toResult m.Reason
        <!> reportFailureWave

  type NewDuelMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable map = Unchecked.defaultof<TerritoryDto>
    
    member m.Location
      with get() = location
      and set(v) = location <- v
    member m.Map
      with get() = map
      and set(v) = map <- v
      
    static member export (wave) =
      let (loc, ampl : NewDuelAmplitude) = wave
      let m = new NewDuelMould ()
      m.Location <- LocationMould.export loc
      m.Map <- TerritoryDto.export ampl.Map
      m
      
    interface Importable with
      member m.import () =
        Ok (fun map ->
          NewDuelAmplitude {
            Map = map;
          })
        <*> TerritoryDto.import m.Map
        <!> newDuelWave

  type StartDuelMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable duel = Unchecked.defaultof<DuelDto>
    let mutable tiles = Unchecked.defaultof<TileWellDto>
    let mutable tileSelections = Unchecked.defaultof<TileSelectionWellDto>
    let mutable pieces = Unchecked.defaultof<PieceWellDto>
    let mutable rules = Unchecked.defaultof<RuleWellDto>
    
    member m.Location
      with get() = location
      and set(v) = location <- v    
    member m.Duel
      with get() = duel
      and set(v) = duel <- v
    member m.Tiles
      with get () = tiles
      and set (v) = tiles <- v
    member m.TileSelections
      with get () = tileSelections
      and set (v) = tileSelections <- v
    member m.Pieces
      with get () = pieces
      and set (v) = pieces <- v
    member m.Rules
      with get () = rules
      and set (v) = rules <- v
      
    static member export (wave) =
      let (loc, ampl : StartDuelAmplitude) = wave
      let m = new StartDuelMould ()
      m.Location <- LocationMould.export loc
      m.Duel <- ampl.Duel |> DuelDto.export
      m
    
    interface Importable with
      member m.import () =
        Ok (fun duel tiles tileSelections pieces rules ->
          StartDuelAmplitude {
            Duel = duel;
            Tiles = tiles;
            TileSelections = tileSelections;
            Pieces = pieces;
            Rules = rules;
          })
        <*> DuelDto.import m.Duel
        <*> TileWellDto.import m.Tiles
        <*> TileSelectionWellDto.import m.TileSelections
        <*> PieceWellDto.import m.Pieces
        <*> RuleWellDto.import m.Rules
        <!> startDuelWave
        
  type JoinDuelMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable id = null

    member m.Location
      with get() = location
      and set(v) = location <- v  
    member m.ID
      with get() = id
      and set(v) = id <- v
      
    static member export (wave) =
      let (loc, ampl : JoinDuelAmplitude) = wave
      let m = new JoinDuelMould ()
      m.Location <- LocationMould.export loc
      m.ID <- ampl.ID
      m
    
    interface Importable with
      member m.import () =
        Ok (fun id ->
          JoinDuelAmplitude {
            ID = id;
          })
        <*> Nullable.toResult m.ID
        <!> addDuelistWave 
      
  type AddDuelistMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable duelist = Unchecked.defaultof<DuelistDto>
    
    member m.Location
      with get() = location
      and set(v) = location <- v    
    member m.Duelist
      with get() = duelist
      and set(v) = duelist <- v  
      
    static member export (wave) =
      let (loc, ampl : AddDuelistAmplitude) = wave
      let m = new AddDuelistMould ()
      m.Location <- LocationMould.export loc
      m.Duelist <- ampl.Duelist |> DuelistDto.export
      m
      
    interface Importable with
      member m.import () =
        Ok (fun duelist ->
          AddDuelistAmplitude {
            Duelist = duelist;
          })
        <*> DuelistDto.import m.Duelist
        <!> addDuelistWave

  type AddPieceMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable piece = Unchecked.defaultof<PieceDto>
    let mutable coordinate = Unchecked.defaultof<CoordinateDto>
    
    member m.Location
      with get() = location
      and set(v) = location <- v    
    member m.Piece
      with get() = piece
      and set(v) = piece <- v  
    member m.Coordinate
      with get() = coordinate
      and set(v) = coordinate <- v  
       
    static member export (wave) =
      let (loc, ampl : AddPieceAmplitude) = wave
      let m = new AddPieceMould ()
      m.Location <- LocationMould.export loc
      m.Piece <- ampl.Piece |> PieceDto.export
      m.Coordinate <- ampl.Coordinate |> CoordinateDto.export
      m
    
    interface Importable with
      member m.import () =
       Ok (fun piece coord ->
          AddPieceAmplitude {
            Piece = piece;
            Coordinate = coord;
          })
        <*> PieceDto.import m.Piece
        <*> CoordinateDto.import m.Coordinate
        <!> addPieceWave
  
  type RemovePieceMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable coordinate = Unchecked.defaultof<CoordinateDto>
    
    member m.Location
      with get() = location
      and set(v) = location <- v    
    member m.Coordinate
      with get() = coordinate
      and set(v) = coordinate <- v    
    
    static member export (wave) =
      let (loc, ampl : RemovePieceAmplitude) = wave
      let m = new RemovePieceMould ()
      m.Location <- LocationMould.export loc
      m.Coordinate <- CoordinateDto.export ampl.Coordinate
      m
    
    interface Importable with
      member m.import () =
        Ok (fun coord ->
          RemovePieceAmplitude {
            Coordinate = coord;
          })
        <*> CoordinateDto.import m.Coordinate
        <!> removePieceWave
        
  type MovePieceMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable piece = Unchecked.defaultof<PieceDto>
    let mutable from = Unchecked.defaultof<CoordinateDto>
    let mutable t = Unchecked.defaultof<CoordinateDto>
    
    member m.Location
      with get() = location
      and set(v) = location <- v    
    member m.Piece
      with get() = piece
      and set(v) = piece <- v   
    member m.From
      with get() = from
      and set(v) = from <- v    
    member m.To
      with get() = t
      and set(v) = t <- v   
      
    static member export (wave) =    
      let (loc, ampl : MovePieceAmplitude) = wave
      let m = new MovePieceMould ()
      m.Location <- LocationMould.export loc
      m.Piece <- ampl.Piece |> PieceDto.export
      m.From <- ampl.From |> CoordinateDto.export
      m.To <- ampl.To |> CoordinateDto.export
      m
    
    interface Importable with
      member m.import () =
        Ok (fun piece from t ->
          MovePieceAmplitude {
            Piece = piece;
            From = from;
            To = t;
          })
        <*> PieceDto.import m.Piece
        <*> CoordinateDto.import m.From
        <*> CoordinateDto.import m.To
        <!> movePieceWave       

  type ConquerTileMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable piece = Unchecked.defaultof<PieceDto>
    let mutable from = Unchecked.defaultof<CoordinateDto>
    let mutable t = Unchecked.defaultof<CoordinateDto>
    
    member m.Location
      with get() = location
      and set(v) = location <- v    
    member m.Piece
      with get() = piece
      and set(v) = piece <- v   
    member m.From
      with get() = from
      and set(v) = from <- v    
    member m.To
      with get() = t
      and set(v) = t <- v   
      
    static member export (wave) =    
      let (loc, ampl : ConquerTileAmplitude) = wave
      let m = new ConquerTileMould ()
      m.Location <- LocationMould.export loc
      m.Piece <- ampl.Piece |> PieceDto.export
      m.From <- ampl.From |> CoordinateDto.export
      m.To <- ampl.To |> CoordinateDto.export
      m

    interface Importable with
      member m.import () =
        Ok (fun piece from t ->
          ConquerTileAmplitude {
            Piece = piece;
            From = from;
            To = t;
          })
        <*> PieceDto.import m.Piece
        <*> CoordinateDto.import m.From
        <*> CoordinateDto.import m.To
        <!> conquerTileWave
        
  type SelectTileMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable player = Unchecked.defaultof<ColorDto>
    let mutable coordinate = Unchecked.defaultof<CoordinateDto>
    
    member m.Location
      with get() = location
      and set(v) = location <- v    
    member m.Player
      with get() = player
      and set(v) = player <- v   
    member m.Coordinate
      with get() = coordinate
      and set(v) = coordinate <- v     
    
    static member export (wave) =
      let (loc, ampl : SelectTileAmplitude) = wave
      let m = new SelectTileMould ()
      m.Location <- LocationMould.export loc
      m.Player <- ampl.Player |> ColorDto.export
      m.Coordinate <- ampl.Coordinate |> CoordinateDto.export
      m

    interface Importable with
      member m.import () =
        Ok (fun player coord ->
          SelectTileAmplitude {
            Player = player;
            Coordinate = coord;
          })
        <*> ColorDto.import m.Player
        <*> CoordinateDto.import m.Coordinate
        <!> selectTileWave
        
  type SelectClientTileMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable coordinate = Unchecked.defaultof<CoordinateDto>
    
    member m.Location
      with get() = location
      and set(v) = location <- v  
    member m.Coordinate
      with get() = coordinate
      and set(v) = coordinate <- v     
    
    static member export (wave) =
      let (loc, ampl : SelectClientTileAmplitude) = wave
      let m = new SelectClientTileMould ()
      m.Location <- LocationMould.export loc
      m.Coordinate <- ampl.Coordinate |> CoordinateDto.export
      m

    interface Importable with
      member m.import () =
        Ok (fun coord ->
          SelectClientTileAmplitude {
            Coordinate = coord;
          })
        <*> CoordinateDto.import m.Coordinate
        <!> selectTileWave
          
  type DeselectTileMould () =
    let mutable location = Unchecked.defaultof<LocationMould>

    member m.Location
      with get() = location
      and set(v) = location <- v 
      
    static member export (wave) =
      let (loc, _ : DefaultAmplitude) = wave
      let m = new DeselectTileMould ()
      m.Location <- LocationMould.export loc
      m

    interface Importable with
      member m.import () =
        DefaultAmplitude ()
        |> deselectTileWave
        |> Ok
        
  type ConfirmDeselectTileMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable player = Unchecked.defaultof<ColorDto>

    member m.Location
      with get() = location
      and set(v) = location <- v    
    member m.Player
      with get() = player
      and set(v) = player <- v
      
    static member export (wave) =
      let (loc, ampl : ConfirmDeselectTileAmplitude) = wave
      let m = new ConfirmDeselectTileMould ()
      m.Location <- LocationMould.export loc
      m.Player <- ampl.Player |> ColorDto.export
      m

    interface Importable with
      member m.import () =
        Ok (fun player ->
          ConfirmDeselectTileAmplitude {
            Player = player;
          })
        <*> ColorDto.import m.Player
        <!> confirmDeselectTileWave
        
  type AddOpenDuelsMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable duels = Array.empty
    
    member m.Location
      with get () = location
      and set (v) = location <- v
    member m.Duels
      with get () = duels
      and set (v) = duels <- v
      
    static member export (wave) =
      let (loc, ampl : AddOpenDuelsAmplitude) = wave
      let m = new AddOpenDuelsMould ()
      m.Location <- LocationMould.export loc
      m.Duels <- List.toArray ampl.Duels
      m
      
    interface Importable with
      member m.import () =
        Ok (fun duels ->
          AddOpenDuelsAmplitude {
            Duels = duels;
          })
        <*> Ok (List.ofArray m.Duels)
        <!> addOpenDuelsWave
                
  let inline make<'t when 't :> Importable> mould  =
    JsonConversions.import<'t> mould <!>> fun (m : 't) -> (m :> Importable).import ()    
        
  let import mould =
    let loc =
      JsonConversions.import<Mould> mould <!>> fun m -> m.import ()
      
    Logger.log loc
      
    loc <!>> function
    | l when l = addTileLocation -> make<AddTileMould> mould
    | l when l = playerCreatedLocation -> make<PlayerCreatedMould> mould
    | l when l = loginLocation -> make<LoginMould> mould
    | l when l = confirmLoginLocation -> make<ConfirmLoginMould> mould
    | l when l = reportFailureLocation -> make<ReportFailureMould> mould
    | l when l = startDuelLocation -> make<StartDuelMould> mould
    | l when l = joinDuelLocation -> make<JoinDuelMould> mould
    | l when l = addDuelistLocation -> make<AddDuelistMould> mould
    | l when l = addPieceLocation -> make<AddPieceMould> mould
    | l when l = removePieceLocation -> make<RemovePieceMould> mould
    | l when l = movePieceLocation -> make<MovePieceMould> mould
    | l when l = conquerTileLocation -> make<ConquerTileMould> mould
    | l when l = selectTileLocation -> make<SelectTileMould> mould
    | l when l = selectClientTileLocation -> make<SelectClientTileMould> mould
    | l when l = deselectTileLocation -> make<DeselectTileMould> mould
    | l when l = confirmDeselectTileLocation -> make<ConfirmDeselectTileMould> mould
    | l when l = addOpenDuelsLocation -> make<AddOpenDuelsMould> mould
    | l -> Error ("Wave location not found: " + (Tuple.fst l).ToString() + ", " + (Tuple.snd l).ToString())
    