namespace ChessPlus

module Clauses =

  let rec areMet conditions rule tileSelectionWell =
    match conditions with
    | Clause (op, c) ->
      ConditionVerification.isMet c rule tileSelectionWell
      |> Operators.isMet op
      
    | AllOf clauses ->
      List.fold (fun acc (op, c) ->
        match acc with
        | Ok true ->
          ConditionVerification.isMet c rule tileSelectionWell
          |> Operators.isMet op
        | falseOrError ->
          falseOrError
      ) (Ok true) clauses
      
    | OneOf clauses ->
      List.fold (fun acc (op, c) ->
        match acc with
        | Ok false ->
          ConditionVerification.isMet c rule tileSelectionWell
          |> Operators.isMet op
        | trueOrError ->
          trueOrError
      ) (Ok false) clauses
      
    | Combination c ->
      List.fold (fun acc innerConditions ->
        match acc with
        | Ok true ->
          areMet innerConditions rule tileSelectionWell
        | falseOrError ->
          falseOrError
      ) (Ok true) c