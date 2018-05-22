namespace ChessPlus

module Clauses =

  let rec areMet conditions rule well =
    Ok true
//    let timer = Logger.time "Clauses.areMet"
//    match conditions with
//    | Clause (op, c) ->
//      ConditionVerification.isMet c rule well
//      |> Operators.isMet op
//      |> timer
//      
//    | AllOf clauses ->
//      List.fold (fun acc (op, c) ->
//        match acc with
//        | Ok true ->
//          ConditionVerification.isMet c rule well
//          |> Operators.isMet op
//        | falseOrError ->
//          falseOrError
//      ) (Ok true) clauses
//      |> timer
//      
//    | OneOf clauses ->
//      List.fold (fun acc (op, c) ->
//        match acc with
//        | Ok false ->
//          ConditionVerification.isMet c rule well
//          |> Operators.isMet op
//        | trueOrError ->
//          trueOrError
//      ) (Ok false) clauses
//      |> timer
//      
//    | Combination c ->
//      List.fold (fun acc innerConditions ->
//        match acc with
//        | Ok true ->
//          areMet innerConditions rule well
//        | falseOrError ->
//          falseOrError
//      ) (Ok true) c
//      |> timer