namespace ChessPlus

module Operators =
  open ConditionVerification
  open Option
  open Types

  let isMet operator conditionResult =
    match operator, conditionResult with
    | Is, Conditional r ->
      Ok r
    | Not, Conditional r ->
      not r |> Ok
    | Equals x, IntValue y ->
      x = y |> Ok
    | GreaterThan x, IntValue y ->
      x > y |> Ok
    | SmallerThan x, IntValue y ->
      x < y |> Ok
    | _, _ ->
      Error "Invalid operator values"