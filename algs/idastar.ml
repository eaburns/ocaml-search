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

  let search info lims _args state =
    let stop = Limit.make_stop lims in
    Printf.printf "searching!\n%!";
    let minoob = ref infinity in
    let bound = ref (D.h state) in
    let goalcost = ref infinity in
    let goal = ref [] in

    let rec dfs pop g st =
      let f = D.h st +. g in
      if f <= !bound then
	if D.is_goal st then begin
	  goal := [ D.dup st ];
	  goalcost := g;
	end else begin
	  dfskids pop g st
	end
      else if f < !minoob then
	minoob := f

    and dfskids pop g st =
      let nops = D.nops st in
      let n = ref 0 in
      info.Info.expd <- info.Info.expd + 1;
      while !n < nops && !goal = [] && not (stop info) do
	let op = D.nthop st !n in
	if op <> pop then begin
	  info.Info.gend <- info.Info.gend + 1;
	  let rev = D.revop st op in
	  let uinfo = D.undoinfo st op in
	  let cost = D.apply st op in
	  dfs rev (cost +. g) st;
	  D.undo st uinfo;
	  if !goal <> [] then
	    goal := D.dup st :: !goal
	end;
	incr n
      done
    in

    try

      info.Info.dups <- -1;
      while !goal = [] && not (stop info) do
	dfs D.nop 0. state;
	Printf.printf "iter: %g, %d, %d\n%!" !bound info.Info.expd
	  info.Info.gend;
	bound := !minoob;
	minoob := infinity;
      done;
      if !goal = [] then None else Some (!goal, !goalcost)

    with Limit_reached -> None
end
