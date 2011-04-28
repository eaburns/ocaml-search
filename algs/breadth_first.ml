module Make (D : Search.Domain)
  : Search.Alg with type state = D.state =
struct

  type state = D.state

  module Ht = Hashtbl.Make (D)

  type node = {
    s : state;
    c : float;
    p : node;
  }

  let rec build_path ?(accum=[]) { s = s; p = p } =
    if s == p.s then s :: accum else build_path ~accum:(s :: accum) p

  let search info lims _args state =
    let q = Queue.create () in
    let goal = ref None in
    let rec init = { s = state; c = 0.; p = init } in
    let stop = Limit.make_stop lims in
    Queue.push init q;
    while not (Queue.is_empty q) && !goal = None && not (stop info) do
      let { s = s; c = c; } as n = Queue.take q in
      if D.is_goal s then
	goal := Some n
      else begin
	info.Info.expd <- info.Info.expd + 1;
	let push (s', dc) =
	  info.Info.gend <- info.Info.gend + 1;
	  Queue.push { s = s'; c = c +. dc; p = n } q in
	List.iter push (D.succs ~parent:n.p.s ~state:s)
      end
    done;
    match !goal with
      | None -> None
      | Some n -> Some (build_path n, n.c)

end
