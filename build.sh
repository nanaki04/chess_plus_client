fsharpc --target:library -r:../Maelstrom/build/Maelstrom.dll -r:/Users/robertjanzwetsloot/Projects/fsharp/ChessPlus/packages/Newtonsoft.Json.6.0.5/lib/net35/Newtonsoft.Json.dll -r:packages/UnityEngine.dll -r:packages/UnityEngine.UI.dll -r:packages/UnityEditor.dll Logging/Logger.fs Config/Env.fs Enhancers/Result.fs Enhancers/Option.fs Enhancers/Col.fs Enhancers/Agent.fs Enhancers/List.fs Enhancers/Map.fs Enhancers/Tuple.fs Enhancers/Matrix.fs Connection/Tcp.fs Connection/Udp.fs Enhancers/Nullable.fs Types.fs Ui/Popup.fs State/Well.fs State/Finders.fs State/Fetchers.fs State/Updaters.fs Enhancers/JsonConvertions.fs Waves/Waves.fs Enhancers/WaveMoulders.fs Logging/WellLogger.fs Logging/WaveLogger.fs Tides/Pool.fs Tides/ConditionVerification.fs Tides/Operators.fs Tides/Clauses.fs Tides/Movements.fs Tides/Tides.fs Flow/Flow.fs Flow/Observers.fs Presentation/Adapters.fs Presentation/Presentation.fs Presentation/DomainRoot.fs Presentation/UiComponent.fs Presentation/Button.fs Presentation/Input.fs Presentation/Popups.fs Presentation/Popups.fs Presentation/Piece.fs Presentation/PieceFactory.fs Presentation/Tile.fs Presentation/Board.fs Presentation/Player.fs Presentation/Login.fs Presentation/UdpStream.fs Presentation/TcpStream.fs
mv TcpStream.dll build/ChessPlus.dll
