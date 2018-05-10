namespace ChessPlus

module Tcp =
  open System.Threading
  open System.Net.Sockets
  open System.Net
  open System.Text

  //let client = new TcpClient("192.168.13.211", 1338)
  let client = new TcpClient("127.0.0.1", 1338)
  let stream = client.GetStream ()
  
  let agent = MailboxProcessor.Start(fun inbox ->
    let rec loop state = async{
      let! update = inbox.Receive()
      return! loop (update state)
    }
    
    loop ""
  )
  
  let thread = new Thread (fun () ->
    let rec awaitMessages () =
      let buffer = Array.create 256 (byte(0))
      let length = stream.Read (buffer, 0, buffer.Length)
      let message = Encoding.ASCII.GetString (buffer, 0, length)
      agent.Post (fun state -> state + message)
      awaitMessages ()
    awaitMessages ()
  )
  
  let listen () =
    thread.Start ()
  
  let send (msg : string) =
    Logger.log "SEND TCP MSG"
    Logger.log(msg)
    let bytes = Encoding.ASCII.GetBytes (msg + "\n")
    stream.Write (bytes, 0, bytes.Length)
        
  let disconnect () =
    thread.Abort ()
    client.Close ()