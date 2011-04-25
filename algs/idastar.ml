(* Iterative deepening A*. *)

module Make (D :
  sig
    include Search.Domain
    include Search.Inplace with type inplace_state = state
  end) =
struct

  exception Goal of D.state list * float

  exception Limit_reached

  let rec dfs info stop bound g state =
    let f = g +. D.h state in
    if stop info then
      raise Limit_reached;
    if f <= bound then begin
      if D.is_goal state then
	raise (Goal (D.path (), g));
      info.Info.expd <- info.Info.expd + 1;
      kids info stop bound g infinity (D.succ_iter ())
    end else
      f

  and kids info stop bound g minoob iter = match D.next iter with
    | None ->
      minoob
    | Some (kid, c) ->
      info.Info.gend <- info.Info.gend + 1;
      let minoob' = min (dfs info stop bound (g +. c) kid) minoob in
      kids info stop bound g minoob' iter

  let finite fl = match classify_float fl with
    | FP_nan -> invalid_arg "Idastar.finite: nan"
    | FP_infinite -> false
    | _ -> true

  let search info lims _orgs state =
    let stop = Limit.make_reached lims in
    try
      let rec iter f =
	let f' = dfs info stop f 0. state in
	if finite f' then iter f' else None in
      iter (D.h state)
    with
      | Goal (path, cost) -> Some (path, cost)
      | Limit_reached -> None

end
