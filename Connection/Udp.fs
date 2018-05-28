namespace ChessPlus

module Udp =
  open System.Net.Sockets
  open System.Net
  open System.Text
  open EnvAccessor

  let client = new UdpClient (env.UdpClientPort)
  //let ip = IPAddress.Parse("192.168.13.211")
  let ip = IPAddress.Parse(env.UdpServerAddress)
  let server = new IPEndPoint (ip, env.UdpServerPort)
  let incomingEndpoint = new IPEndPoint (IPAddress.Any, 0)
  
  let agent = MailboxProcessor.Start(fun inbox ->
    let rec loop state = async{
      let! update = inbox.Receive()
      return! loop (update state)
    }
    
    loop []
  )

  let rec listen () =
    let cb = new System.AsyncCallback (fun r ->
      client.EndReceive (r, ref incomingEndpoint)
      |> Encoding.ASCII.GetString
      |> (fun msg -> agent.Post (fun state -> msg::state))
      listen ()
    )
    client.BeginReceive (cb, ()) |> ignore
  
  let connect () =
    client.Connect (server)
    listen ()
      
  let send (msg : string) =
    let bytes = Encoding.ASCII.GetBytes (msg)
    client.Send (bytes, bytes.Length)
        
  let disconnect () = client.Close ()
