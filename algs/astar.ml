module Make (D : Search.Domain)
  : Search.Alg with type state = D.state =
struct

  type state = D.state

  module Ht = Hashtbl.Make (D)

  type node = {
    s : state;
    mutable g : float;
    mutable f : float;
    mutable p : node;
    mutable pq_pos : int;
  }

  let no_pos = Heap.no_pos

  let rec build_path ?(accum=[]) { s = s; p = p } =
    if s == p.s then s :: accum else build_path ~accum:(s :: accum) p

  let is_better a b =
    if a.f = b.f then a.g > b.g else a.f < b.f

  let update_pq_pos a i =
    a.pq_pos <- i

  let update_node opn ~node ~parent g =
    node.f <- (node.f -. node.g) +. g;
    node.g <- g;
    node.p <- parent;
    if node.pq_pos = no_pos then
      Heap.push opn node
    else
      Heap.update_key opn node.pq_pos

  let handle_children info opn cls n =
    let handle_child (s', dg) =
      info.Info.gend <- info.Info.gend + 1;
      let g' = n.g +. dg in
      try
	let n' = Ht.find cls s' in
	info.Info.dups <- info.Info.dups + 1;
	if n'.g > g' then update_node opn ~node:n' ~parent:n g'
      with Not_found ->
	let n' =
	  { s = s'; p = n; g = g'; f = D.h s' +. g'; pq_pos = no_pos } in
	Heap.push opn n';
	Ht.add cls s' n'
    in
    info.Info.expd <- info.Info.expd + 1;
    List.iter handle_child (D.succs ~parent:n.p.s ~state:n.s)

  let search info lims _args state =
    let rec init =
      { s = state; p = init;g = 0.; f = D.h state; pq_pos = no_pos } in
    let opn = Heap.init ~index:update_pq_pos is_better [||] in
    let cls = Ht.create 149 in
    let goal = ref None in
    let stop = Limit.make_stop lims in
    Heap.push opn init;
    Ht.add cls state init;
    while not (Heap.is_empty opn) && !goal = None && not (stop info) do
      let { s = s; } as n = Heap.pop opn in
      if D.is_goal s then goal := Some n else handle_children info opn cls n
    done;
    match !goal with None -> None | Some n -> Some (build_path n, n.g)

end
