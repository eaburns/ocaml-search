(** A sliding tiles solver. *)

(* Disallow any confusion between positions and contents arrays. *)
module Ary : sig
  type contents = private string

  type positions = private string

  val positions : int -> positions
  val contents : int -> contents
  val set_pos : positions -> int -> int -> unit
  val get_pos : positions -> int -> int
  val set_cont : contents -> int -> int -> unit
  val get_cont : contents -> int -> int

end = struct
  type contents = string

  type positions = string

  let positions size = String.create size

  let contents size = String.create size

  let set str ind vl =
    (* Chang.unsafe_chr = better performance and worse debugging.*)
    str.[ind] <- Char.chr vl

  let get str ind =
    Char.code str.[ind]

  let set_pos = set
  let get_pos = get
  let set_cont = set
  let get_cont = get
end

open Ary

type state = {
  ary : contents;
  h : float;
}

type inst = {
  rows : int;
  cols : int;
  init : contents;
  goal : positions;
}

(** Create the canonical goal position set for each tile. *)
let goal_pos : row:int -> col:int -> positions = fun ~row ~col ->
  let size = row * col in
  let pos = positions size in
  for i = 0 to size - 1 do set_pos pos i i done;
  pos

let korf_12 =
  let s = contents 16 in
  set_cont s 0 14;
  set_cont s 1 1;
  set_cont s 2 9;
  set_cont s 3 6;
  set_cont s 4 4;
  set_cont s 5 8;
  set_cont s 6 12;
  set_cont s 7 5;
  set_cont s 8 7;
  set_cont s 9 2;
  set_cont s 10 3;
  set_cont s 11 0;
  set_cont s 12 10;
  set_cont s 13 11;
  set_cont s 14 13;
  set_cont s 15 15;
  { rows = 4;
    cols = 4;
    init = s;
    goal = goal_pos 4 4; }

(** Build a table that gives the Manhattan distance from its goal
    position for each tile given each location. *)
let md_table inst =
  let rows = inst.rows in
  let goal = inst.goal in
  let size = rows * inst.cols in
  let tab = Array.make_matrix size size 0 in
  for tile = 1 to size - 1 do
    let locs = tab.(tile) in
    for loc = 0 to size - 1 do
      let srow = loc / rows and scol = loc mod rows in
      let dloc = get_pos goal tile in
      let drow = dloc / rows and dcol = dloc mod rows in
      locs.(tile) <- abs (drow - srow) + abs (dcol - scol);
    done
  done;
  tab

(** Make a goal test function. *)
let make_is_goal inst =
  let size = inst.rows * inst.cols in
  let goal = inst.goal in
  let rec in_pos contents pos =
    if pos < size then begin
      let tile = get_cont contents pos in
      get_pos goal tile = pos && in_pos contents (pos + 1)
    end else
      true in
  (fun state -> in_pos state.contents 0)
