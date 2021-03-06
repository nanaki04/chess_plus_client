﻿namespace ChessPlus

module Clauses =

  let rec areMet conditions rule piece isSimulation tileSelectionWell =
    match conditions with
    | Clause c ->
      ConditionVerification.isMet c rule piece isSimulation tileSelectionWell
      
    | AllOf clauses ->
      List.fold (fun acc c ->
        match acc with
        | Ok true ->
          ConditionVerification.isMet c rule piece isSimulation tileSelectionWell
        | falseOrError ->
          falseOrError
      ) (Ok true) clauses
      
    | OneOf clauses ->
      List.fold (fun acc c ->
        match acc with
        | Ok false ->
          ConditionVerification.isMet c rule piece isSimulation tileSelectionWell
        | trueOrError ->
          trueOrError
      ) (Ok false) clauses
      
    | Combination c ->
      List.fold (fun acc innerConditions ->
        match acc with
        | Ok true ->
          areMet innerConditions rule piece isSimulation tileSelectionWell
        | falseOrError ->
          falseOrError
      ) (Ok true) c