namespace ChessPlus

module Tuple =
  let map1st<'t1, 't2, 'r> f (fst : 't1) (snd : 't2) : 'r * 't2 =
    (f fst, snd)
    
  let map2nd<'t1, 't2, 'r> f (fst : 't1) (snd : 't2) : 't1 * 'r =
    (fst, f snd)
    
  let fst<'t1, 't2> (a : 't1, b : 't2) : 't1 = a

  let snd<'t1, 't2> (a : 't1, b : 't2) : 't2 = b