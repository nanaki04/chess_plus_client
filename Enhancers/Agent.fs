namespace ChessPlus

module Agent =
  type T<'T> (initialValue) =
    let mutable state : 'T = initialValue
    
    member m.Call update =
      state <- update state

  let start<'T> initializer =
    let agent = new T<'T> (initializer ())
    agent.Call