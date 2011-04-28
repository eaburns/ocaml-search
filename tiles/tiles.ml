(** A sliding tiles solver. *)

(* Disallow any confusion between positions and contents arrays. *)

open Tiles_inst

type oper = Left | Right | Up | Down | No_op

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

(** Make a function to compute the Manhattan distance of a contents
    array. *)
let md inst mdtab : contents ary -> float =
  let size = inst.rows * inst.cols in
  let sum = ref 0 in
  (fun c ->
    sum := 0;
    for pos = 0 to size - 1 do
      let tile = get c pos in
      if tile > 0 then
	sum := !sum + mdtab.(tile).(pos)
    done;
    float !sum)

(** Compute the Manhattan distance heuristic incrementally given the
    parent heuristic value [h], the parent's blank position [blnk], the
    new blank position [blnk] and the new contents array [contents']. *)
let md_incr inst mdtab : float -> int -> int -> contents ary -> float =
  (fun h blnk blnk' contents' ->
    let tile = get contents' blnk in
    let delta = mdtab.(tile).(blnk) - mdtab.(tile).(blnk') in
    h +. float delta)

(** Make a goal test function on a contents array. *)
let make_is_goal inst : contents ary -> bool =
  let size = inst.rows * inst.cols in
  let goal = inst.goal in
  let rec in_pos contents pos =
    if pos < size then begin
      let tile = get contents pos in
      get goal tile = pos && in_pos contents (pos + 1)
    end else
      true in
  (fun contents -> in_pos contents 0)

(** Test if [op] is applicable with a blank in the given position. *)
let applicable inst blnk op =
  let cols = inst.cols in
  let row = blnk / cols and col = blnk mod cols in
  match op with
    | Left -> col > 0
    | Right -> col < cols - 1
    | Up -> row > 0
    | Down -> row < inst.rows - 1
    | No_op -> true

(** Applies [op] to [cont], where the blank is in position [blnk].
    The result is the new blank location. *)
let apply inst : int -> contents ary -> oper -> int =
  (fun blnk cont op ->
    let cols = inst.cols in
    let blnk' = match op with
      | Left -> blnk - 1
      | Right -> blnk + 1
      | Up -> blnk - cols
      | Down -> blnk + cols
      | No_op -> blnk in
    let tile = get cont blnk' in
    set cont blnk tile;
    set cont blnk' 0;
    blnk')

(** Get the operator that transitions the blank from [blnk] to
    [blnk']. *)
let get_op inst blnk blnk' =
  let cols = inst.cols in
  if blnk' = blnk - cols then
    Up
  else if blnk' = blnk + cols then
    Down
  else if blnk' = blnk - 1 then
    Left
  else if blnk' = blnk + 1 then
    Right
  else
    No_op

(** Reverse an operator. *)
let rev_op = function
  | Left -> Right
  | Right -> Left
  | Up -> Down
  | Down -> Up
  | No_op -> No_op

(** Scan through a contents array to find the blank's position. *)
let blank_pos inst conts =
  let size = inst.rows * inst.cols in
  let rec find_blnk i =
    if i >= size then
      invalid_arg "Tiles.blank_pos: no blank"
    else
      if get conts i = 0 then i else find_blnk (i + 1) in
  find_blnk 0

(** {1 Search functionality} *)

type state = {
  contents : contents ary;
  mutable blnk : int;
  mutable h : float;
}

type t = state

type inplace_state = state

type iter = {
  op : oper;
  mutable nxt : int;
}

let hash a = Hashtbl.hash a.contents

let equal a b = equal a.contents b.contents

let opers = [| Left; Right; Up; Down |]

let fmt inst fmt state =
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
  Format.fprintf fmt "h=%g@\n" state.h

let fmt_op fmt = function
  | Left -> Format.fprintf fmt "Left"
  | Right -> Format.fprintf fmt "Right"
  | Up -> Format.fprintf fmt "Up"
  | Down -> Format.fprintf fmt "Down"
  | No_op -> Format.fprintf fmt "No_op"

let succs inst md_incr ~parent ~state =
  let p_op = get_op inst state.blnk parent.blnk in
  let consider_op lst op =
    if op = p_op || not (applicable inst state.blnk op) then
      lst
    else begin
      let conts = copy state.contents in
      let blnk = apply inst state.blnk conts op in
      let kid =
	{ contents = conts;
	  blnk = blnk;
	  h = md_incr state.h state.blnk blnk conts; } in
      (kid, 1.) :: lst
    end in
  Array.fold_left consider_op [] opers

let h state =
  state.h

let d state =
  state.h

let succ_iter inst gen_op =
  let op = match gen_op with None -> No_op | Some o -> rev_op o in
  { op = op; nxt = Array.length opers - 1; }

(** Make a next-state function for the given iterator over the given
    in-place state. *)
let rec next inst md_incr state it =
  if it.nxt >= 0 then begin
    let op = opers.(it.nxt) in
    it.nxt <- it.nxt - 1;
    let conts = state.contents and blnk = state.blnk in
    if op <> it.op && (applicable inst blnk op) then
      let blnk' = apply inst blnk conts op in
      let h' = md_incr state.h blnk blnk' conts in
      state.blnk <- blnk';
      state.h <- h';
      Some (1., op)
    else
      next inst md_incr state it
  end else
    None

(** Undo the given operator. *)
let rec undo inst md_incr state op =
  let rev = rev_op op in
  let conts = state.contents and blnk = state.blnk in
  let blnk' = apply inst blnk conts rev in
  let h' = md_incr state.h blnk blnk' conts in
  state.blnk <- blnk';
  state.h <- h'

(** Duplicate the current in-place state. *)
let dup state =
  { state with contents = copy state.contents; }

let op inst a b =
  get_op inst a.blnk b.blnk
