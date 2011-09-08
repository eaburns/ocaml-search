(** Main program for the sliding tile solver. *)

open Printf

let alg_tbl inst incrtab =
  let module D = struct
    include Tiles

    let is_goal state = state.h = 0

    let d s = truncate (d s)

    let succs = succs inst incrtab

    let nops = nops inst

    let nthop = nthop inst

    let apply = apply incrtab

    let fmt = fmt inst

  end in
  (let module T = Algtab.Domain(D) in T.table)
  @ (let module T = Algtab.Inplace(D) in T.table)

let time f =
  let stime = Sys.time () in
  let r = f () in
  r, Sys.time () -. stime

let args () =
  let lims = ref [] in
  let alg = ref "" in
  let args = ref [||] in
  let alg_arg s =
    if !alg = "" then
      alg := s
    else
      let l = Array.length !args in
      args := Array.init (l + 1) (fun i -> if i < l then (!args).(i) else s) in
  let spec =
    Limit.arg_spec lims @ [ "-c", Arg.String (fun _ -> ()), "ignored" ] in
  Arg.parse spec alg_arg "./tiles_main <alg> [<arg>...]";
  !lims, !alg, !args

let _ =
  let lims, alg, args = args () in
  let inst = Tiles_inst.read stdin in
  let mdtab = Tiles.md_table inst in
  let incrtab = Tiles.md_incr_tab inst mdtab in
  let tbl = alg_tbl inst incrtab in
  let search = List.assoc alg tbl in
  let init =
    { Tiles.contents = inst.Tiles_inst.init;
      blnk = Tiles.blank_pos inst inst.Tiles_inst.init;
      h = Tiles.md inst mdtab inst.Tiles_inst.init; } in
  let info = Info.create () in
  let cost, time =
    time (fun () -> match search info lims args init with
      | None -> infinity
      | Some (path, cost) ->
	printf "#pair  \"final sol length\" \"%d\"\n" (List.length path);
	cost) in
  printf "#pair  \"final sol cost\" \"%g\"\n" cost;
  printf "#pair  \"total raw cpu time\" \"%g\"\n" time;
  Info.output stdout info;
  Limit.output stdout lims
