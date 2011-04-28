(** Main program for the sliding tile solver. *)

open Printf

let alg_tbl inst mdtab =
  let module D = struct
    include Tiles

    let is_goal =
      let is_goal = make_is_goal inst in
      (fun state -> is_goal state.Tiles.contents)

    let d s = truncate (d s)

    let md = md_incr inst mdtab

    let succs = succs inst md

    let succ_iter = succ_iter inst

    let next = next inst md

    let undo = undo inst md

    let fmt = fmt inst

  end in
  (let module T = Algtab.Domain(D) in T.table)
  @ (let module T = Algtab.Inplace(D) in T.table)

let time f =
  let stime = Sys.time () in
  let r = f () in
  r, Sys.time () -. stime

let _ =
  let alg = Sys.argv.(1) in
  let inst = Tiles_inst.read stdin in
  let mdtab = Tiles.md_table inst in
  let tbl = alg_tbl inst mdtab in
  let search = List.assoc alg tbl in
  let init =
    { Tiles.contents = inst.Tiles_inst.init;
      blnk = Tiles.blank_pos inst inst.Tiles_inst.init;
      h = Tiles.md inst mdtab inst.Tiles_inst.init; } in
  let info = Info.create () in
  let lims = [] in
  let cost, time =
    time (fun () -> match search info lims [||] init with
      | None -> infinity
      | Some (path, cost) ->
	printf "#pair  \"final sol length\" \"%d\"\n" (List.length path);
	cost) in
  printf "#pair  \"final sol cost\" \"%g\"\n" cost;
  printf "#pair  \"total raw cpu time\" \"%g\"\n" time;
  Info.output stdout info;
  Limit.output stdout lims