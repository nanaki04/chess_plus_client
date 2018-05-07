namespace ChessPlus

module Udp =
  open System.Net.Sockets
  open System.Net
  open System.Text

  let client = new UdpClient (5000)
  let ip = IPAddress.Parse("127.0.0.1")
  let server = new IPEndPoint (ip, 1337)
  let incomingEndpoint = new IPEndPoint (IPAddress.Any, 0)
  
  let connect () =
    client.Connect (server)

  let rec listen handleMessage =
    let cb = new System.AsyncCallback (fun r ->
      client.EndReceive (r, ref incomingEndpoint)
      |> Encoding.ASCII.GetString
      |> handleMessage
      listen handleMessage
    )
    client.BeginReceive (cb, ()) |> ignore
  
  let send (msg : string) =
    let bytes = Encoding.ASCII.GetBytes (msg)
    client.Send (bytes, bytes.Length)
        
  let disconnect () = client.Close ()
