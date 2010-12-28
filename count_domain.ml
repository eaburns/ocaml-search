(** Some clumbsy domain that just counts numbers up and down until it
    hits some goal value.

    @author eaburns
    @since 2010-12-15
*)

type state = int
type t = state
type metric_state = state
type reversable_state = state

let succs s =
  [ s + 1, 1.;
    s - 1, ~-.1.; ]

let preds s = succs s

let make_is_goal goal_val s =
  s = goal_val

let make_d goal_val s =
  abs (s - goal_val)

let make_h goal_val s =
  float (make_d goal_val s)

let hash i = (i:int)

let equal a b = (a:int) = b

let dist i j = abs (i - j)

let cost i j = float (dist i j)
