(* Iterative deepening A*. *)

module Make (D :
  sig
    include Search.Domain
    include Search.Inplace with type inplace_state = state
  end) : Search.Alg with type state = D.state =
struct

  type state = D.state

  exception Limit_reached

  let rec dfs info stop bound g ?gen_op ~state =
    let f = g +. D.h state in
    if stop info then
      raise Limit_reached;
    if f <= bound then begin
      if D.is_goal state then
	[D.dup ()], g
      else begin
	info.Info.expd <- info.Info.expd + 1;
	let iter = D.succ_iter gen_op in
	let goal, cost = kids info stop bound g infinity state iter in
	if goal = [] then [], cost else D.dup () :: goal, cost
      end
    end else
      [], f

  and kids info stop bound g minoob gen_op iter = match D.next iter with
    | None ->
      [], minoob
    | Some (kid, c, op) ->
      info.Info.gend <- info.Info.gend + 1;
      let goal, cost = dfs info stop bound (g +. c) ~gen_op:op ~state:kid in
      D.undo op;
      if goal = [] then
	kids info stop bound g (min cost minoob) gen_op iter
      else
	goal, cost

  let finite fl = match classify_float fl with
    | FP_nan -> invalid_arg "Idastar.finite: nan"
    | FP_infinite -> false
    | _ -> true

  let search info lims _orgs state =
    let stop = Limit.make_reached lims in
    try
      let rec iter f =
	let goal, cost = dfs info stop f 0. state in
	if goal = [] then
	  if finite cost then iter cost else None
	else
	  Some (goal, cost) in
      iter (D.h state)
    with Limit_reached -> None
end
