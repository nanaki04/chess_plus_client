fsharpc --target:library -r:../Maelstrom/build/Maelstrom.dll -r:/Users/robertjanzwetsloot/Projects/fsharp/ChessPlus/packages/Newtonsoft.Json.6.0.5/lib/net35/Newtonsoft.Json.dll -r:packages/UnityEngine.dll -r:packages/UnityEngine.UI.dll -r:packages/UnityEditor.dll Logging/Logger.fs Config/Env.fs Enhancers/Result.fs Enhancers/Option.fs Enhancers/Col.fs Enhancers/Agent.fs Enhancers/List.fs Enhancers/Map.fs Enhancers/Tuple.fs Enhancers/Matrix.fs Connection/Tcp.fs Connection/Udp.fs Connection/VisionQuestTcp.fs Enhancers/Nullable.fs Types.fs Ui/Popup.fs State/Well.fs Enhancers/JsonConvertions.fs State/Finders.fs State/Fetchers.fs State/Updaters.fs Waves/Waves.fs Enhancers/WaveMoulders.fs Logging/WellLogger.fs Logging/WaveLogger.fs Logging/VisionQuest.fs Tides/Pool.fs Tides/Operators.fs Tides/ConditionVerification.fs Tides/Clauses.fs Tides/Rules.fs Tides/Movements.fs Tides/Conquers.fs Tides/Tides.fs Flow/Flow.fs Flow/Observers.fs Presentation/Adapters.fs Presentation/Presentation.fs Presentation/InputHandler.fs Presentation/DomainRoot.fs Presentation/UiComponent.fs Presentation/Button.fs Presentation/Input.fs Presentation/Popups.fs Presentation/Popups.fs Presentation/IPiece.fs Presentation/Piece3D.fs Presentation/Piece.fs Presentation/PieceFactory.fs Presentation/ITile.fs Presentation/Tile.fs Presentation/Tile3D.fs Presentation/Board.fs Presentation/Player.fs Presentation/Login.fs Presentation/DuelState.fs Presentation/UdpStream.fs Presentation/TcpStream.fs
mv TcpStream.dll build/ChessPlus.dll
