(** Sample main program for the counting domain.

    @author eaburns
    @since 2010-12-15
*)

open Printf

let _ =
  let alg = Sys.argv.(1) in
  let goal = int_of_string Sys.argv.(2) in
  let module D = struct
    include Count_domain
    let is_goal = make_is_goal goal
  end in
  let module T = Alg_table.Make(D) in
  let search = T.lookup alg in
    match search 1 with
      | None -> printf "No solution found\n"
      | Some (path, cost) ->
	  printf "cost: %f\n" cost;
	  List.iter (printf "%d\n") path
