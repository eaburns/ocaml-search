module Make (D : Search.Domain)
  : Search.Alg with type state = D.state =
struct

  type state = D.state

  module Ht = Hashtbl.Make (D)

  type node = {
    s : state;
    h : float;
    mutable p : node;
    mutable g : float;
    mutable f : float;
    mutable pq_pos : int;
  }

  let no_pos = Dpq.no_position

  let rec build_path ?(accum=[]) { s = s; p = p } =
    if s == p.s then s :: accum else build_path ~accum:(s :: accum) p

  let is_better a b =
    if a.f = b.f then a.g > b.g else a.f < b.f

  let update_pq_pos a i =
    a.pq_pos <- i

  let update_node o n n' g' =
    (** [update_node o n n' g'] updates a node given a new g value
	[g']. *)
    n'.g <- g';
    n'.f <- n'.h +. g';
    n'.p <- n;
    if n'.pq_pos = no_pos then
      Dpq.insert o n'
    else
      Dpq.see_update o n'.pq_pos

  let handle_children o c n =
    (** [handle_children o c n] expands [n] and deals with its
	children. *)
    let handle_child (s', dg) =
      let g' = n.g +. dg in
      try
	let n' = Ht.find c s' in
	if n'.g > g' then update_node o n n' g'
      with Not_found ->
	let h = D.h s' in
	let n' =
	  { s = s'; p = n; h = h; g = g'; f = h +. g'; pq_pos = no_pos } in
	Dpq.insert o n';
	Ht.add c s' n'
    in
    List.iter handle_child (D.succs ~parent:n.p.s ~state:n.s)

  let search state =
    (** [search state] A* search starting at [state]. *)
    let h = D.h state in
    let rec init =
      { s = state; p = init; h = h; g = 0.; f = h; pq_pos = no_pos } in
    let o = Dpq.create is_better update_pq_pos 1024 init in
    let c = Ht.create 149 in
    let goal = ref None in
    Dpq.insert o init;
    Ht.add c state init;
    while not (Dpq.empty_p o) && !goal = None do
      let { s = s; } as n = Dpq.extract_first o in
      if D.is_goal s then goal := Some n else handle_children o c n
    done;
    match !goal with
      | None -> None
      | Some n -> Some (build_path n, n.g)

end
