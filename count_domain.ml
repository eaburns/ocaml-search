(** Some clumbsy domain that just counts numbers up and down until it
    hits some goal value.

    @author eaburns
    @since 2010-12-15
*)

type state = int
type t = state

let expand s =
  [ s + 1, 1.;
    s - 1, ~-.1.; ]

let make_is_goal goal_val s =
  s = goal_val

let hash i = (i:int)

let equal a b = (a:int) = b
