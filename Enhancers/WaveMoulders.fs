namespace ChessPlus
open JsonConversions
open DtoTypes
open Waves

module Moulds =
  open Result
  open System
  open Types

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
 
  type DefaultMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    
    member m.Location
      with get() = location
      and set(v) = location <- v
    
    static member export (wave) =
      let (loc, ampl : DefaultAmplitude) = wave
      let m = new DefaultMould ()
      m.Location <- LocationMould.export loc
      m
      
    interface Importable with
      member m.import () =
        Nullable.toResult m.Location
        >>= fun loc -> loc.import ()
        <!> fun loc -> (loc, DefaultAmplitude ())
      
  type TextMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable text = Unchecked.defaultof<string>
    
    member m.Location
      with get () = location
      and set (v) = location <- v  
    member m.Text
      with get () = text
      and set (v) = text <- v
      
    static member export (wave) =
      let (loc, ampl : TextAmplitude) = wave
      let m = new TextMould ()
      m.Location <- LocationMould.export loc
      m.Text <- ampl.Text
      m
      
    interface Importable with
      member m.import () =
        Nullable.toResult m.Location
        >>= fun loc -> loc.import ()
        <!> fun loc -> (loc, TextAmplitude { Text = m.Text })    
         
  type UiComponentMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable uiComponent = Unchecked.defaultof<UiComponentDto>
    
    member m.Location
      with get () = location
      and set (v) = location <- v  
    member m.UiComponent
      with get () = uiComponent
      and set (v) = uiComponent <- v
      
    static member export (wave) =
      let (loc, ampl : UiComponentAmplitude) = wave
      let m = new UiComponentMould ()
      m.Location <- LocationMould.export loc
      m.UiComponent <- JsonConversions.UiComponentDto.export ampl
      m
      
    interface Importable with
      member m.import () =
        Nullable.toResult m.Location
        >>= fun loc -> loc.import ()
        >>= fun loc ->
          JsonConversions.UiComponentDto.import m.UiComponent
          <!> fun uiComponent -> (loc, UiComponentAmplitude uiComponent)
        
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
    let mutable buffs = Unchecked.defaultof<BuffWellDto>
    
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
    member m.Buffs
      with get () = buffs
      and set (v) = buffs <- v
      
    static member export (wave) =
      let (loc, ampl : StartDuelAmplitude) = wave
      let m = new StartDuelMould ()
      m.Location <- LocationMould.export loc
      m.Duel <- ampl.Duel |> DuelDto.export
      m.Tiles <- ampl.Tiles |> TileWellDto.export
      m.TileSelections <- ampl.TileSelections |> TileSelectionWellDto.export
      m.Pieces <- ampl.Pieces |> PieceWellDto.export
      m.Rules <- ampl.Rules |> RuleWellDto.export
      m.Buffs <- ampl.Buffs |> BuffWellDto.export
      m
    
    interface Importable with
      member m.import () =
        Ok (fun duel tiles tileSelections pieces rules buffs ->
          StartDuelAmplitude {
            Duel = duel;
            Tiles = tiles;
            TileSelections = tileSelections;
            Pieces = pieces;
            Rules = rules;
            Buffs = buffs;
          })
        <*> DuelDto.import m.Duel
        <*> TileWellDto.import m.Tiles
        <*> TileSelectionWellDto.import m.TileSelections
        <*> PieceWellDto.import m.Pieces
        <*> RuleWellDto.import m.Rules
        <*> BuffWellDto.import m.Buffs
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
            Piece = Types.Pieces.update (fun p -> { p with Coordinate = Some coord }) piece;
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
            Piece = Types.Pieces.update (fun p -> { p with Coordinate = Some from }) piece;
            From = from;
            To = t;
          })
        <*> PieceDto.import  m.Piece
        <*> CoordinateDto.import m.From
        <*> CoordinateDto.import m.To
        <!> movePieceWave   
            
  type PromotePieceMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable piece = Unchecked.defaultof<PieceDto>
    
    member m.Location
      with get() = location
      and set(v) = location <- v    
    member m.Piece
      with get() = piece
      and set(v) = piece <- v 
      
    static member export (wave) =    
      let (loc, ampl : PromotePieceAmplitude) = wave
      let m = new PromotePieceMould ()
      m.Location <- LocationMould.export loc
      m.Piece <- ampl.Piece |> PieceDto.export
      m
    
    interface Importable with
      member m.import () =
        Ok (fun piece ->
          PromotePieceAmplitude {
            Piece = piece;
          })
        <*> PieceDto.import  m.Piece
        <!> promotePieceWave 
 
  type UpdateBuffsMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable buffs = Unchecked.defaultof<BuffWellDto>
    
    member m.Location
      with get() = location
      and set(v) = location <- v
    member m.Buffs
      with get() = buffs
      and set(v) = buffs <- v
      
    static member export (wave) =
      let (loc, ampl : UpdateBuffsAmplitude) = wave
      let m = new UpdateBuffsMould ()
      m.Location <- LocationMould.export loc
      m.Buffs <- ampl.Buffs |> BuffWellDto.export
      m
    
    interface Importable with
      member m.import () =
        Ok (fun buffs ->
          UpdateBuffsAmplitude {
            Buffs = buffs;
          })
        <*> BuffWellDto.import m.Buffs
        <!> updateBuffsWave
          
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
            Piece = Types.Pieces.update (fun p -> { p with Coordinate = Some from }) piece;
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
        
  type UpdateDuelStateMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable duelState = Unchecked.defaultof<DuelStateDto>
    
    member m.Location
      with get () = location
      and set (v) = location <- v
    member m.DuelState
      with get () = duelState
      and set (v) = duelState <- v
      
    static member export (wave) =
      let (loc, ampl : UpdateDuelStateAmplitude) = wave
      let m = new UpdateDuelStateMould ()
      m.Location <- LocationMould.export loc
      m.DuelState <- DuelStateDto.export ampl.DuelState
      m
      
    interface Importable with
      member m.import () =
        Ok (fun duelState ->
          UpdateDuelStateAmplitude {
            DuelState = duelState;
          })
        <*> DuelStateDto.import m.DuelState
        <!> updateDuelStateWave
        
  type UpdateTcpConnectionMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable connected = Unchecked.defaultof<bool>
    
    member m.Location
      with get () = location
      and set (v) = location <- v
    member m.Connected
      with get () = connected
      and set (v) = connected <- v
      
    static member export (wave) =
      let (loc, ampl : UpdateTcpConnectionAmplitude) = wave
      let m = new UpdateTcpConnectionMould ()
      m.Location <- LocationMould.export loc
      m.Connected <- ampl.Connected
      m
      
    interface Importable with
      member m.import () =
        Ok (fun connected ->
          UpdateTcpConnectionAmplitude {
            Connected = connected;
          })
        <*> (Ok m.Connected)
        <!> updateTcpConnectionWave

  type UpdateUdpConnectionMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable connected = Unchecked.defaultof<bool>
    
    member m.Location
      with get () = location
      and set (v) = location <- v
    member m.Connected
      with get () = connected
      and set (v) = connected <- v
      
    static member export (wave) =
      let (loc, ampl : UpdateUdpConnectionAmplitude) = wave
      let m = new UpdateUdpConnectionMould ()
      m.Location <- LocationMould.export loc
      m.Connected <- ampl.Connected
      m
      
    interface Importable with
      member m.import () =
        Ok (fun connected ->
          UpdateUdpConnectionAmplitude {
            Connected = connected;
          })
        <*> (Ok m.Connected)
        <!> updateUdpConnectionWave
   
  type OpenPopupMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable popup = Unchecked.defaultof<PopupDto>
    
    member m.Location
      with get () = location
      and set (v) = location <- v
    member m.Popup
      with get () = popup
      and set (v) = popup <- v
      
    static member export (wave) =
      let (loc, ampl : OpenPopupAmplitude) = wave
      let m = new OpenPopupMould ()
      m.Location <- LocationMould.export loc
      m.Popup <- PopupDto.export ampl.Popup
      m
      
    interface Importable with
      member m.import () =
        Ok (fun popup ->
          OpenPopupAmplitude {
            Popup = popup;
          })
        <*> (PopupDto.import m.Popup)
        <!> openPopupWave   

  type ClosePopupMould () =
    let mutable location = Unchecked.defaultof<LocationMould>
    let mutable popup = Unchecked.defaultof<PopupDto>
    
    member m.Location
      with get () = location
      and set (v) = location <- v
    member m.Popup
      with get () = popup
      and set (v) = popup <- v
      
    static member export (wave) =
      let (loc, ampl : ClosePopupAmplitude) = wave
      let m = new ClosePopupMould ()
      m.Location <- LocationMould.export loc
      m.Popup <- PopupDto.export ampl.Popup
      m
      
    interface Importable with
      member m.import () =
        Ok (fun popup ->
          ClosePopupAmplitude {
            Popup = popup;
          })
        <*> (PopupDto.import m.Popup)
        <!> closePopupWave
        
  let exportMould wave =
    match wave with
    | (loc, UpdateTcpConnectionAmplitude ampl) -> UpdateTcpConnectionMould.export (loc, ampl) |> JsonConversions.export |> Ok
    | (loc, UpdateUdpConnectionAmplitude ampl) -> UpdateUdpConnectionMould.export (loc, ampl) |> JsonConversions.export |> Ok
    | (loc, PlayerCreatedAmplitude ampl) -> PlayerCreatedMould.export (loc, ampl) |> JsonConversions.export |> Ok
    | (loc, ConfirmLoginAmplitude ampl) -> ConfirmLoginMould.export (loc, ampl) |> JsonConversions.export |> Ok
    | (loc, ReportFailureAmplitude ampl) -> ReportFailureMould.export (loc, ampl) |> JsonConversions.export |> Ok
    | (loc, StartDuelAmplitude ampl) -> StartDuelMould.export (loc, ampl) |> JsonConversions.export |> Ok
    | (loc, AddDuelistAmplitude ampl) -> AddDuelistMould.export (loc, ampl) |> JsonConversions.export |> Ok
    | (loc, UpdateDuelStateAmplitude ampl) -> UpdateDuelStateMould.export (loc, ampl) |> JsonConversions.export |> Ok
    | (("player", "in"), DefaultAmplitude _ampl) -> Mould.export ("player", "in") |> JsonConversions.export |> Ok
    | (loc, OpenPopupAmplitude ampl) -> OpenPopupMould.export (loc, ampl) |> JsonConversions.export |> Ok
    | (loc, ClosePopupAmplitude ampl) -> ClosePopupMould.export (loc, ampl) |> JsonConversions.export |> Ok
    | (("login_popup", "click_ok"), DefaultAmplitude _ampl) -> Mould.export ("login_popup", "click_ok") |> JsonConversions.export |> Ok
    | (("login_popup", "change_name_text"), TextAmplitude ampl) -> TextMould.export (("login_popup", "change_name_text"), ampl) |> JsonConversions.export |> Ok
    | (("play_duel_popup", "new_button"), UiComponentAmplitude ampl) -> UiComponentMould.export (("play_duel_popup", "new_button"), ampl) |> JsonConversions.export |> Ok
    | (("play_duel_popup", "join_button"), UiComponentAmplitude ampl) -> UiComponentMould.export (("play_duel_popup", "join_button"), ampl) |> JsonConversions.export |> Ok
    | (("play_duel_popup", "click_join"), DefaultAmplitude _ampl) -> Mould.export ("play_duel_popup", "click_join") |> JsonConversions.export |> Ok
    | (("play_duel_popup", "click_new"), DefaultAmplitude _ampl) -> Mould.export ("play_duel_popup", "click_new") |> JsonConversions.export |> Ok
    | (loc, AddOpenDuelsAmplitude ampl) -> AddOpenDuelsMould.export (loc, ampl) |> JsonConversions.export |> Ok
    | (loc, ConquerTileAmplitude ampl) -> ConquerTileMould.export (loc, ampl) |> JsonConversions.export |> Ok
    | (loc, SelectTileAmplitude ampl) -> SelectTileMould.export (loc, ampl) |> JsonConversions.export |> Ok
    | (loc, PromotePieceAmplitude ampl) -> PromotePieceMould.export (loc, ampl) |> JsonConversions.export |> Ok
    | (loc, SelectClientTileAmplitude ampl) -> SelectClientTileMould.export (loc, ampl) |> JsonConversions.export |> Ok
    | (("tile", "deselect"), DefaultAmplitude _ampl) -> Mould.export ("tile", "deselect") |> JsonConversions.export |> Ok
    | (loc, ConfirmDeselectTileAmplitude ampl) -> ConfirmDeselectTileMould.export (loc, ampl) |> JsonConversions.export |> Ok
    | (loc, MovePieceAmplitude ampl) -> MovePieceMould.export (loc, ampl) |> JsonConversions.export |> Ok
    | (loc, AddPieceAmplitude ampl) -> AddPieceMould.export (loc, ampl) |> JsonConversions.export |> Ok
    | (loc, RemovePieceAmplitude ampl) -> RemovePieceMould.export (loc, ampl) |> JsonConversions.export |> Ok
    | (loc, UpdateBuffsAmplitude ampl) -> UpdateBuffsMould.export (loc, ampl) |> JsonConversions.export |> Ok
    | (("white_player", "information_plate"), DefaultAmplitude _ampl) -> Mould.export ("white_player", "information_plate") |> JsonConversions.export |> Ok
    | (("black_player", "information_plate"), DefaultAmplitude _ampl) -> Mould.export ("black_player", "information_plate") |> JsonConversions.export |> Ok
    | (("white_player", "name"), DefaultAmplitude _ampl) -> Mould.export ("white_player", "name") |> JsonConversions.export |> Ok
    | (("black_player", "name"), DefaultAmplitude _ampl) -> Mould.export ("black_player", "name") |> JsonConversions.export |> Ok
    | (("game_menu", "menu"), DefaultAmplitude _ampl) -> Mould.export ("game_menu", "menu") |> JsonConversions.export |> Ok
    | (("game_menu", "click_forfeit"), DefaultAmplitude _ampl) -> Mould.export ("game_menu", "click_forfeit") |> JsonConversions.export |> Ok
    | (("game_menu", "click_remise"), DefaultAmplitude _ampl) -> Mould.export ("game_menu", "click_remise") |> JsonConversions.export |> Ok
    | (("duelist", "forfeit"), DefaultAmplitude _ampl) -> Mould.export ("duelist", "forfeit") |> JsonConversions.export |> Ok
    | (("duelist", "propose_remise"), DefaultAmplitude _ampl) -> Mould.export ("duelist", "propose_remise") |> JsonConversions.export |> Ok
    | (("duelist", "remise"), DefaultAmplitude _ampl) -> Mould.export ("duelist", "remise") |> JsonConversions.export |> Ok
    | (("duelist", "refuse_remise"), DefaultAmplitude _ampl) -> Mould.export ("duelist", "refuse_remise") |> JsonConversions.export |> Ok
    | (("confirm_remise_popup", "click_yes"), DefaultAmplitude _ampl) -> Mould.export ("confirm_remise_popup", "click_yes") |> JsonConversions.export |> Ok
    | (("confirm_remise_popup", "click_no"), DefaultAmplitude _ampl) -> Mould.export ("confirm_remise_popup", "click_no") |> JsonConversions.export |> Ok
    | (("refuse_remise_popup", "click_ok"), DefaultAmplitude _ampl) -> Mould.export ("refuse_remise_popup", "click_ok") |> JsonConversions.export |> Ok
    | ((domain, invocation), _) -> Error ("Unmatched Wave: " + domain + ":" + invocation)
                  
  let inline make<'t when 't :> Importable> mould  =
    JsonConversions.import<'t> mould <!>> fun (m : 't) -> (m :> Importable).import ()
        
  let import mould =
    let loc =
      JsonConversions.import<Mould> mould <!>> fun m -> m.import ()
      
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
    | l when l = promotePieceLocation -> make<PromotePieceMould> mould
    | l when l = selectTileLocation -> make<SelectTileMould> mould
    | l when l = selectClientTileLocation -> make<SelectClientTileMould> mould
    | l when l = deselectTileLocation -> make<DeselectTileMould> mould
    | l when l = confirmDeselectTileLocation -> make<ConfirmDeselectTileMould> mould
    | l when l = addOpenDuelsLocation -> make<AddOpenDuelsMould> mould
    | l when l = updateDuelStateLocation -> make<UpdateDuelStateMould> mould
    | l when l = whitePlayerInformationPlateLocation -> make<DefaultMould> mould
    | l when l = blackPlayerInformationPlateLocation -> make<DefaultMould> mould
    | l when l = whitePlayerNameDynamicText -> make<DefaultMould> mould
    | l when l = blackPlayerNameDynamicText -> make<DefaultMould> mould
    | l when l = gameMenuLocation -> make<DefaultMould> mould
    | l when l = gameMenuClickForfeitButtonLocation -> make<DefaultMould> mould
    | l when l = gameMenuClickRemiseButtonLocation -> make<DefaultMould> mould
    | l when l = forfeitDuelLocation -> make<DefaultMould> mould
    | l when l = proposeRemiseLocation -> make<DefaultMould> mould
    | l when l = remiseLocation -> make<DefaultMould> mould
    | l when l = refuseRemiseLocation -> make<DefaultMould> mould
    | l when l = confirmRemisePopupClickYesButtonLocation -> make<DefaultMould> mould
    | l when l = confirmRemisePopupClickNoButtonLocation -> make<DefaultMould> mould
    | l when l = remiseRefusedPopupClickOkButtonLocation -> make<DefaultMould> mould
    | l -> Error ("Wave location not found: " + (Tuple.fst l).ToString() + ", " + (Tuple.snd l).ToString())
    