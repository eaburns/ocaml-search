  (* Iterative deepening A*. *)

let finite fl = match classify_float fl with
  | FP_nan -> invalid_arg "Idastar.finite: nan"
  | FP_infinite -> false
  | _ -> true

(* This in-place version may be slower than the out-of-place version
   because mutating record fields in OCaml may be expensive (hash table
   lookup).  On domains where the states are small (and easy to copy),
   the out-of-place version will actually be faster. *)
module Inplace (D :
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
	[D.dup state], g
      else begin
	info.Info.expd <- info.Info.expd + 1;
	let iter = D.succ_iter gen_op in
	let goal, cost = kids info stop bound g infinity state iter in
	if goal = [] then [], cost else D.dup state :: goal, cost
      end
    end else
      [], f

  and kids info stop bound g minoob state iter =
    match D.next state iter with
      | None ->
	[], minoob
      | Some (c, op) ->
	info.Info.gend <- info.Info.gend + 1;
	let goal, cost = dfs info stop bound (g +. c) ~gen_op:op ~state in
	D.undo state op;
	if goal = [] then
	  kids info stop bound g (min cost minoob) state iter
	else
	  goal, cost

  let search info lims _orgs state =
    let stop = Limit.make_stop lims in
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

module Outplace (D : Search.Domain) : Search.Alg with type state = D.state =
struct

  type state = D.state

  exception Limit_reached

  let rec dfs info stop bound g ~parent ~state =
    let f = g +. D.h state in
    if stop info then
      raise Limit_reached;
    if f <= bound then begin
      if D.is_goal state then
	[state], g
      else begin
	let succs = D.succs ~parent ~state in
	info.Info.expd <- info.Info.expd + 1;
	info.Info.gend <- info.Info.gend + List.length succs;
	let goal, cost = kids info stop bound g infinity state succs in
	if goal = [] then [], cost else state :: goal, cost
      end
    end else
      [], f

  and kids info stop bound g minoob parent = function
    | [] ->
      [], minoob
    | (k, c) :: ks ->
      let goal, cost = dfs info stop bound (g +. c) ~parent ~state:k in
      if goal = [] then
	kids info stop bound g (min cost minoob) parent ks
      else
	goal, cost


  let search info lims _orgs state =
    let stop = Limit.make_stop lims in
    try
      let rec iter f =
	let goal, cost = dfs info stop f 0. ~parent:state ~state in
	if goal = [] then
	  if finite cost then iter cost else None
	else
	  Some (goal, cost) in
      iter (D.h state)
    with Limit_reached -> None
end
