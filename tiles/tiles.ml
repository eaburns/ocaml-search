(** A sliding tiles solver. *)

(* Disallow any confusion between positions and contents arrays. *)

open Tiles_inst

type oper = int

(** Build a table that gives the Manhattan distance from its goal
    position for each tile given each location. *)
let md_table inst =
  let cols = inst.cols in
  let goal = inst.goal in
  let size = cols * inst.rows in
  let tab = Array.make_matrix size size 0 in
  for tile = 1 to size - 1 do
    let locs = tab.(tile) in
    for sloc = 0 to size - 1 do
      let srow = sloc / cols and scol = sloc mod cols in
      let dloc = get goal tile in
      let drow = dloc / cols and dcol = dloc mod cols in
      locs.(sloc) <- abs (drow - srow) + abs (dcol - scol);
    done
  done;
  tab

let md_incr_tab inst md_table =
  let size = inst.cols * inst.rows in
  let inc = Array.make size [||] in
  for tile = 1 to size - 1 do
    let mds = md_table.(tile) in
    let incrs = Array.make_matrix size size 0 in
    inc.(tile) <- incrs;
    for src = 0 to size - 1 do
      for dst = 0 to size - 1 do
	incrs.(src).(dst) <- mds.(dst) - mds.(src)
      done
    done
  done;
  inc

(** Make a function to compute the Manhattan distance of a contents
    array. *)
let md inst mdtab : contents ary -> int =
  let size = inst.rows * inst.cols in
  let sum = ref 0 in
  (fun c ->
    sum := 0;
    for pos = 0 to size - 1 do
      let tile = get c pos in
      if tile > 0 then
	sum := !sum + mdtab.(tile).(pos)
    done;
    !sum)

(** Compute the Manhattan distance heuristic incrementally given the
    parent heuristic value [h], the parent's blank position [blnk], the
    new blank position [blnk] and the new contents array [contents']. *)
let md_incr inst incrtab : float -> int -> int -> contents ary -> float =
  (fun h blnk blnk' contents' ->
    let tile = get contents' blnk in
    let delta = incrtab.(tile).(blnk').(blnk) in
    h +. float delta)

(** {1 Search functionality} *)

type state = {
  contents : contents ary;
  mutable blnk : int;
  mutable h : int;
}

type t = state

type inplace_state = state

type undoinfo = int * int

let hash a = Hashtbl.hash a.contents

let equal a b = equal a.contents b.contents

let fmt inst fmt state =
  set state.contents state.blnk 0;
  for r = 0 to inst.rows - 1 do
    let b = r * inst.cols in
    for c = 0 to inst.cols - 1 do
      let i = b + c in
      let t = get state.contents i in
      if c > 0 then Format.fprintf fmt "\t";
      Format.fprintf fmt "%3d" t;
    done;
    Format.fprintf fmt "@\n";
  done;
  Format.fprintf fmt "h=%d@\n" state.h

let succs inst incrtab ~parent ~state =
  let kids = ref [] in
  let b = state.blnk in
  for i = inst.ops.(b).n - 1 downto 0 do
    let b' = inst.ops.(b).mvs.(i) in
    let t = get state.contents b' in
    if b' <> parent.blnk then begin
      let kid = {
	contents = copy state.contents;
	blnk = b';
	h = state.h + incrtab.(t).(b').(b)
      } in
      set kid.contents b t;
      kids := (kid, 1.0) :: !kids
    end
  done;
  !kids

let h state =
  float state.h

let d state =
  float state.h

let nop = -1

let nops inst state =
  inst.ops.(state.blnk).n

let nthop inst state n =
  inst.ops.(state.blnk).mvs.(n)

let revop state op =
  state.blnk

let apply incrtab state b' =
  let b = state.blnk in
  let t = get state.contents b' in
  state.blnk <- b';
  state.h <- state.h + incrtab.(t).(b').(b);
  set state.contents b t;
  1.0

let undoinfo state op =
  (state.h, state.blnk)

(** Undo the given operator. *)
let rec undo state (h, b) =
  state.h <- h;
  set state.contents state.blnk (get state.contents b);
  state.blnk <- b

(** Duplicate the current in-place state. *)
let dup state =
  { state with contents = copy state.contents; }

let blank_pos inst cont =
  let b = ref (-1) in
  for i = 0 to inst.rows * inst.cols - 1 do
    if get cont i = 0 then
      b := i
  done;
  !b
