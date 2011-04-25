(** A sliding tiles solver. *)

(* Disallow any confusion between positions and contents arrays. *)
module Ary : sig
  type positions
  type contents
  type 'a ary = private string

  val positions : int -> positions ary
  val contents : int -> contents ary
  val set : 'a ary -> int -> int -> unit
  val get : 'a ary -> int -> int
  val swap : 'a ary -> int -> int -> unit
  val copy : 'a ary -> 'a ary
  val equal : 'a ary -> 'a ary -> bool

end = struct
  type contents

  type positions

  type 'a ary = string

  let positions size = String.create size

  let contents size = String.create size

  let set str ind vl =
    (* Chang.unsafe_chr = better performance and worse debugging.*)
    str.[ind] <- Char.chr vl

  let get str ind =
    Char.code str.[ind]

  let swap str a b =
    let t = get str a in
    set str a (get str b);
    set str b t

  let copy = String.copy

  let equal a b = String.compare a b = 0
end

open Ary

type inst = {
  rows : int;
  cols : int;
  init : contents ary;
  goal : positions ary;
}

type oper = Left | Right | Up | Down | No_op

(** Create the canonical goal position set for each tile. *)
let goal_pos : row:int -> col:int -> positions ary = fun ~row ~col ->
  let size = row * col in
  let pos = positions size in
  for i = 0 to size - 1 do set pos i i done;
  pos

let korf_12 =
  let s = contents 16 in
  set s 0 14;
  set s 1 1;
  set s 2 9;
  set s 3 6;
  set s 4 4;
  set s 5 8;
  set s 6 12;
  set s 7 5;
  set s 8 7;
  set s 9 2;
  set s 10 3;
  set s 11 0;
  set s 12 10;
  set s 13 11;
  set s 14 13;
  set s 15 15;
  { rows = 4;
    cols = 4;
    init = s;
    goal = goal_pos 4 4; }

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

(** Make a function to compute the Manhattan distance a contents array. *)
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
  let size = inst.rows * inst.cols in
  (fun blnk cont op ->
    assert (applicable inst blnk op);
    assert (get cont blnk = 0);
    let cols = inst.cols in
    let blnk' = match op with
      | Left -> blnk - 1
      | Right -> blnk + 1
      | Up -> blnk - cols
      | Down -> blnk + cols
      | No_op -> blnk in
    assert (blnk' < size);
    swap cont blnk blnk';
    assert (get cont blnk' = 0);
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
  blnk : int;
  h : float;
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

let succ_iter inst blnk parent =
  { op = get_op inst !blnk parent.blnk;
    nxt = 0; }

(** Make a next-state function for the given iterator over the given
    in-place state. *)
let rec next inst md_incr blnk h conts it =
  if it.nxt < Array.length opers then begin
    let op = opers.(it.nxt) in
    it.nxt <- it.nxt + 1;
    if (applicable inst !blnk op) && op <> it.op then
      let blnk' = apply inst !blnk conts op in
      let h' = md_incr !h !blnk blnk' conts in
      blnk := blnk';
      h := h';
      Some ({ contents = conts; blnk = blnk'; h = !h; }, 1., op)
    else
      next inst md_incr blnk h conts it
  end else
    None

(** Undo the given operator. *)
let rec undo inst md_incr blnk h conts op =
  let rev = rev_op op in
  assert (applicable inst !blnk rev);
  let blnk' = apply inst !blnk conts rev in
  let h' = md_incr !h !blnk blnk' conts in
  blnk := blnk';
  h := h'

(** Duplicate the current in-place state. *)
let dup blnk h conts () =
  { contents = copy conts; blnk = !blnk; h = !h; }

(** Make a set of functions that operate on a single state using
    in-place modification. *)
let inplace inst mdtab =
  let md_incr = md_incr inst mdtab in
  let conts = copy inst.init in
  let blnk = ref (blank_pos inst conts) in
  let h = ref (md inst mdtab conts) in
  succ_iter inst blnk,
  next inst md_incr blnk h conts,
  undo inst md_incr blnk h conts,
  dup blnk h conts
