(** Grid path-finding search. *)

open Grid_inst

type state = int
type t = state

let hash = Hashtbl.hash

let equal (a:state) b = a = b

let make_h inst =
  let xg = inst.xg and yg = inst.yg in
  let loc = loc inst.h in
  match inst.costs, inst.moves with
    | Unit, Four_way ->
      (fun id -> let x, y = loc id in float (abs (xg - x) + abs (yg - y)))
    | _, _ ->
      invalid_arg "Unimplemented costs/moves combination"

let make_d inst =
  let xg = inst.xg and yg = inst.yg in
  let loc = loc inst.h in
  match inst.moves with
    | Four_way ->
      (fun id -> let x, y = loc id in (abs (xg - x) + abs (yg - y)))
    | Eight_way ->
      invalid_arg "Eight_way d function is not implemented"

let make_is_goal inst =
  let gid = id inst.h inst.xg inst.yg in
  (fun id -> id = gid)

let four_way_succs loc id blkd cost w h =
  let kids = ref [] in
  let child parent state x y =
    let id = id x y in
    if id <> parent && not blkd.(id) then
      kids := (id, cost state id) :: !kids; in
  (fun ~parent ~state ->
    kids := [];
    let x, y = loc state in
    let top = y = 0 and bottom = y = h - 1 in
    let left = x = 0 and right = x = w - 1 in
    if not top then child parent state x (y - 1);
    if not bottom then child parent state x (y + 1);
    if not left then child parent state (x - 1) y;
    if not right then child parent state (x + 1) y;
    !kids)

let unit_cost _ _ = 1.

let make_succs inst =
  let loc = loc inst.h and id = id inst.h and blkd = inst.blkd in
  let cost = unit_cost and w = inst.w and h = inst.h in
  match inst.moves with
    | Four_way -> four_way_succs loc id blkd cost w h
    | Eight_way -> invalid_arg "Eight_way expansion is not yet implemented"
