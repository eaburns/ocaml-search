(** Sample main program for the counting domain.

    @author eaburns
    @since 2010-12-15
*)

open Printf

let alg_tbl goal =
  let module D = struct
    include Count_domain
    let is_goal = make_is_goal goal
    let d = make_d goal
    let h = make_h goal
  end in
  (let module T = Alg_table.Domain(D) in T.table)

let _ =
  let alg = Sys.argv.(1) in
  let goal = int_of_string Sys.argv.(2) in
  let tbl = alg_tbl goal in
  let search = List.assoc alg tbl in
    match search 1 with
      | None -> printf "No solution found\n"
      | Some (path, cost) ->
	  printf "cost: %f\n" cost;
	  List.iter (printf "%d\n") path
