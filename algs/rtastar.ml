
module type Eval = sig
  type state
  type res = Goal of state list * float | Val of float

  (** [eval stop depth_limit state] *)
  val eval : (Info.t -> bool) -> Info.t -> int -> state -> res
end

let minf (a:float) b = if a < b then a else b
let comparef (a:float) b = if a = b then 0 else if a < b then ~-1 else 1

(* Perform DFS using inplace modification. *)
module Inplace_eval(D :
  sig
    include Search.Domain
    include Search.Inplace with type inplace_state = state
  end) : Eval with type state = D.state =
struct
  type state = D.state
  type res = Goal of state list * float | Val of float

  let eval _stop _info _dlim _state = Val nan
end

(* Perform DFS using non-inplace methods. *)
module Outplace_eval(D : sig include Search.Domain end) :
  Eval with type state = D.state =
struct
  type state = D.state
  type res = Goal of state list * float | Val of float

  let eval stop info dlim state =

    let rec dfs alpha g depth ~parent ~state =
      let f = g +. D.h state in
      if D.is_goal state then
	Goal ([state], g)
      else if not (stop info) && depth < dlim && f < alpha then begin
	let succs = D.succs ~parent ~state in
	info.Info.expd <- info.Info.expd + 1;
	info.Info.gend <- info.Info.gend + (List.length succs);
	match kids alpha g (depth + 1) state succs with
	  | Goal (path, cost) -> Goal (state :: path, cost)
	  | vl -> vl
      end else
	Val (minf alpha f)

    and kids alpha g depth' parent = function
      | (k, c) :: ks ->
	begin match dfs alpha (g +. c) depth' ~parent ~state:k with
	  | Goal _ as g -> g
	  | Val v -> kids (minf alpha v) g depth' parent ks
	end
      | [] -> Val alpha in

    dfs infinity 0. 0 ~parent:state ~state

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
      Some (List.rev (state :: path), cost)
    else begin
      let kids = ref (D.succs ~parent:state ~state) in
      info.Info.expd <- info.Info.expd + 1;
      info.Info.gend <- info.Info.gend + (List.length !kids);
      let goal = ref None in
      let mins = ref [] and minvl = ref infinity in
      let sndvl = ref infinity in
      while !goal = None && !kids <> [] do
	let (k, c) = List.hd !kids in
	kids := List.tl !kids;
	match h k with
	  | E.Goal (p, c) ->
	    goal := Some (List.rev (p @ path), c +. cost)
	  | E.Val h ->
	    let v = h +. c in
	    if v < !minvl then begin
	      sndvl := !minvl;
	      minvl := v;
	      mins := [k, c];
	    end else if v = !minvl then
	      mins := (k, c) :: !mins
	    else if v < !sndvl then
	      sndvl := v
      done;
      match !goal with
	| Some _ -> !goal
	| None when !mins = [] -> None	(* dead-end *)
	| None ->
	  Ht.replace seen state !sndvl;
	  let len = List.length !mins in
	  let n = if len = 1 then 0 else Random.int len in
	  let k, c = List.nth !mins n in
	  rtastar stop info h seen (k :: path) (cost +. c) k
    end

  let h stop info dlim seen state =
    try
      E.Val (Ht.find seen state)
    with Not_found ->
      match E.eval stop info dlim state with
	| (E.Val v) as vl ->
	  Ht.add seen state v;
	  vl
	| E.Goal _ as g -> g

  let search info lims args init =
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
