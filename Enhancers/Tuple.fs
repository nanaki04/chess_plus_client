namespace ChessPlus

module Tuple =
  let retn2<'t1, 't2> (v1 : 't1) (v2 : 't2) = (v1, v2)

  let map1st<'t1, 't2, 'r> f (fst : 't1) (snd : 't2) : 'r * 't2 =
    (f fst, snd)
    
  let map2nd<'t1, 't2, 'r> f (fst : 't1) (snd : 't2) : 't1 * 'r =
    (fst, f snd)
    
  let fst<'t1, 't2> (a : 't1, b : 't2) : 't1 = a

  let snd<'t1, 't2> (a : 't1, b : 't2) : 't2 = b

  let toList2 (a, b) = [a; b]
  
  let fromList2 lst =
    match List.length lst with
    | 2 -> Ok (lst.[0], lst.[1])
    | _ -> Error "Incorrect list length to create a tuple 2"
    
  let toArray2 (a, b) = [|a;b|]
  
  let fromArray2 arr =
    match Array.length arr with
    | 2 -> Ok (arr.[0], arr.[0])
    | _ -> Error "Incorrect array length to create a tuple 2"