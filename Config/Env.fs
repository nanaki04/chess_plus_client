﻿namespace ChessPlus

open UnityEngine

type Env () =
  inherit ScriptableObject ()
  
  [<SerializeField>]
  let mutable udpClientPort = 5000
  [<SerializeField>]
  let mutable udpServerAddress = "127.0.0.1"
  [<SerializeField>]
  let mutable udpServerPort = 1337
  [<SerializeField>]
  let mutable tcpServerAddress = "127.0.0.1"
  [<SerializeField>]
  let mutable tcpServerPort = 1338
  
  member m.UdpClientPort
    with get () = udpClientPort
  member m.UdpServerAddress
    with get () = udpServerAddress
  member m.UdpServerPort
    with get () = udpServerPort
  member m.TcpServerAddress
    with get () = tcpServerAddress
  member m.TcpServerPort
    with get () = tcpServerPort

module EnvAccessor =
  let env = UnityEngine.Resources.Load ("Env") :?> Env