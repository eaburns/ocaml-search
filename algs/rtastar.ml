
module type Eval = sig
  type state

  (** [eval stop depth_limit state] *)
  val eval : (Info.t -> bool) -> Info.t -> int -> state -> float
end

let minf (a:float) b = if a < b then a else b

(* Perform DFS using inplace modification. *)
module Inplace_eval(D :
  sig
    include Search.Domain
    include Search.Inplace with type inplace_state = state
  end) : Eval with type state = D.state =
struct
  type state = D.state

  let eval _stop _info _dlim _state = nan
end

(* Perform DFS using non-inplace methods. *)
module Outplace_eval(D : sig include Search.Domain end) :
  Eval with type state = D.state =
struct
  type state = D.state

  let eval stop info dlim state =

    let rec dfs ~alpha ~g depth ~parent ~state =
      if D.is_goal state then
	g
      else begin
	let f = g +. D.h state in
	if depth < dlim && f < alpha && not (stop info) then begin
	  let succs = D.succs ~parent ~state in
	  info.Info.expd <- info.Info.expd + 1;
	  info.Info.gend <- info.Info.gend + (List.length succs);
	  best_kid ~alpha ~g (depth + 1) state succs
	end else
	  minf alpha f
      end

    and best_kid ~alpha ~g depth' parent = function
      | (k, c) :: ks ->
	let v = dfs ~alpha ~g:(g +. c) depth' ~parent ~state:k in
	best_kid ~alpha:(minf alpha v) ~g depth' parent ks
      | [] -> alpha in

    dfs ~alpha:infinity ~g:0. 0 ~parent:state ~state

end

(* The core of the RTA* search algorithm. *)
module Make(E : Eval) (D : Search.Domain with type state = E.state) :
  Search.Alg with type state = D.state =
struct
  type state = D.state

  module Ht = Hashtbl.Make(D)

  let rec rtastar stop info h seen path cost state =
    if stop info then
      None
    else if D.is_goal state then
      Some (List.rev path, cost)
    else begin
      let kids = D.succs ~parent:state ~state in
      info.Info.expd <- info.Info.expd + 1;
      info.Info.gend <- info.Info.gend + (List.length kids);
      let mins = ref [] and minf = ref infinity and sndf = ref infinity in
      let consider_kid ((k, c) as kid) =
	let f = h k +. c in
	if f < !minf then begin
	  sndf := !minf;
	  minf := f;
	  mins := [kid];
	end else if f = !minf then
	  mins := kid :: !mins
	else if f < !sndf then
	  sndf := f in
      List.iter consider_kid kids;
      if !mins = [] then
	None
      else begin
	Ht.replace seen state !sndf;
	let len = List.length !mins in
	let n = if len = 1 then 0 else Random.int len in
	let k, c = List.nth !mins n in
	rtastar stop info h seen (k :: path) (cost +. c) k
      end
    end

  let h stop info dlim seen state =
    try
      let h = Ht.find seen state in
      info.Info.dups <- info.Info.dups + 1;
      h
    with Not_found ->
      let h = E.eval stop info dlim state in
      Ht.add seen state h;
      h

  let search info lims args init =
    Random.self_init ();
    let dlim = int_of_string args.(0) in
    let stop = Limit.make_stop lims in
    let seen = Ht.create 149 in
    let h = h stop info dlim seen in
    rtastar stop info h seen [init] 0. init
end

(* The inplace RTA* search module. *)
module Inplace (D :
  sig
    include Search.Domain
    include Search.Inplace with type inplace_state = state
  end) = Make(Inplace_eval(D))(D)

(* The out-of-place (normal?) RTA* search module. *)
module Outplace (D : Search.Domain) = Make(Outplace_eval(D))(D)
