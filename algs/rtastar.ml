(* RTA*.  There are 2 versions: one uses in-place modification and one
   uses out-of-place modification. *)

let minf (a:float) b = if a < b then a else b

(* The core of the RTA* search algorithm. *)
module Make (D : Search.Domain) =
struct
  type state = D.state

  module Ht = Hashtbl.Make(D)

  let rec rtastar stop info h seen path cost ~parent ~state =
    if stop info then
      None
    else if D.is_goal state then
      Some (List.rev path, cost)
    else begin
      let kids = D.succs ~parent ~state in
      info.Info.expd <- info.Info.expd + 1;
      info.Info.gend <- info.Info.gend + List.length kids;
      let mins = ref [] and minf = ref infinity and sndf = ref infinity in
      let consider_kid ((k, c) as kid) =
	let f = h ~parent:state ~state:k +. c in
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
	rtastar stop info h seen (k :: path) (cost +. c)
	  ~parent:state ~state:k
      end
    end

  let h eval stop info dlim seen ~parent ~state =
    try
      let h = Ht.find seen state in
      info.Info.dups <- info.Info.dups + 1;
      h
    with Not_found ->
      let h = eval stop info dlim ~parent ~state in
      Ht.add seen state h;
      h

  let search eval info lims args init =
    let dlim = int_of_string args.(0) in
    if Array.length args > 1 then
      Random.init (int_of_string args.(1))
    else
      Random.self_init ();
    let stop = Limit.make_stop lims in
    let seen = Ht.create 149 in
    let h = h eval stop info dlim seen in
    rtastar stop info h seen [init] 0. ~parent:init ~state:init
end

(* The inplace RTA* search module. *)
module Inplace (D :
  sig
    include Search.Domain
    include Search.Inplace with type inplace_state = state
  end) : Search.Alg with type state = D.state =
struct

  let eval stop info dlim ~parent ~state =

    let rec dfs ~alpha ~g depth gen_op state =
      if D.is_goal state then
	g
      else begin
	let f = g +. D.h state in
	if depth < dlim && f < alpha && not (stop info) then begin
	  let iter = D.succ_iter (Some gen_op) in
	  info.Info.expd <- info.Info.expd + 1;
	  best_kid ~alpha ~g (depth + 1) state iter
	end else
	  minf alpha f
      end

    and best_kid ~alpha ~g depth' state iter =
      match D.next state iter with
	| None -> alpha
	| Some (c, op) ->
	  info.Info.gend <- info.Info.gend + 1;
	  let v = dfs ~alpha ~g:(g +. c) depth' op state in
	  D.undo state op;
	  best_kid ~alpha:(minf alpha v) ~g depth' state iter in

    dfs ~alpha:infinity ~g:0. 0 (D.op parent state) state

  module Rtastar = Make(D)
  include Rtastar

  let search = search eval

end

(* The out-of-place RTA* search module. *)
module Outplace (D : Search.Domain) : Search.Alg with type state = D.state =
struct

  let eval stop info dlim ~parent ~state =

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

    dfs ~alpha:infinity ~g:0. 0 ~parent ~state

  module Rtastar = Make(D)
  include Rtastar

  let search = search eval

end
