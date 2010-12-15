(** Simple implementation of breadth-first search.

    @author eaburns
    @since 2010-12-15
*)

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

  let search state =
    let q = Queue.create () in
    let goal = ref None in
    let rec init = { s = state; c = 0.; p = init } in
      Queue.push init q;
      while not (Queue.is_empty q) && !goal = None do
	let { s = s; c = c; } as n = Queue.take q in
	  if D.is_goal s then
	    goal := Some n
	  else
	    List.iter
	      (fun (s', dc) ->
		 Queue.push { s = s'; c = c +. dc; p = n } q)
	      (D.expand s)
      done;
      match !goal with
	| None -> raise Not_found
	| Some n -> build_path n, n.c

end
