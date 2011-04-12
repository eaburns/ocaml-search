(** Some clumbsy domain that just counts numbers up and down until it
    hits some goal value.

    @author eaburns
    @since 2010-12-15
*)

type state = int
type t = state
type metric_state = state
type revable_state = state
type rev_ops_state = state
type typed_state = state

module Operator = struct
  type t = Up | Down

  let equal a = function
    | Up when a = Up -> true
    | Down when a = Down -> true
    | _ -> false

  let hash = function
    | Up -> 0
    | Down -> 1
end

module State_type = struct
  type t = Positive | Negative

  let to_int = function
    | Positive -> 0
    | Negative -> 1
end

let t s =
  if s < 0 then
    State_type.Negative
  else
    State_type.Positive

let succs_ops s =
  [ s + 1, Operator.Up, 1.;
    s - 1, Operator.Down, ~-.1.; ]

let preds_ops s = succs_ops s

let succs ~parent ~state =
  if parent = state + 1 then
    [ state - 1, ~-.1. ]
  else if parent = state - 1 then
    [ state + 1, 1. ]
  else
    [ state + 1, 1.; state - 1, ~-.1. ]

let preds state = succs ~parent:state ~state

let make_is_goal goal_val s =
  s = goal_val

let make_d goal_val s =
  abs (s - goal_val)

let make_h goal_val s =
  float (make_d goal_val s)

let hash i = (i:int)

let equal a b = (a:int) = b

let dist ~src ~dst = abs (src - dst)

let cost ~src ~dst = float (dist dst dst)
