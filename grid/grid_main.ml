(** Main program for the grid path-finding domain. *)

open Printf

let alg_tbl inst =
  let module D = struct
    include Grid
    let is_goal = make_is_goal inst
    let d = make_d inst
    let h = make_h inst
    let succs = make_succs inst
  end in
  (let module T = Alg_table.Domain(D) in T.table)

let warm_gc inst =
(*
  let words = inst.Grid_inst.w * inst.Grid_inst.h * 128 in
  Gc.set
    { (Gc.get ()) with
      Gc.minor_heap_size = words;
      Gc.space_overhead = 8192; }
*)()

let time f =
  let stime = Sys.time () in
  let r = f () in
  r, Sys.time () -. stime

let _ =
  let alg = Sys.argv.(1) in
  let inst = Grid_inst.read stdin in
  let tbl = alg_tbl inst in
  let search = List.assoc alg tbl in
  let init =
    let x0 = inst.Grid_inst.x0 and y0 = inst.Grid_inst.y0 in
    Grid_inst.id inst.Grid_inst.h x0 y0 in
  ignore (warm_gc inst);
  let info = Info.create () in
  let lims = [] in
  let cost, time =
    time (fun () -> match search info lims [||] init with
      | None -> infinity
      | Some (path, cost) ->
	printf "length: %d\n" (List.length path);
	cost) in
  printf "cost: %g\n" cost;
  printf "time: %g seconds\n" time;
  Info.output stdout info;
  Limit.output stdout lims
