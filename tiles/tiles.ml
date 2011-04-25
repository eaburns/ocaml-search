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
end

open Ary

type state = {
  ary : contents ary;
  h : float;
}

type inst = {
  rows : int;
  cols : int;
  init : contents ary;
  goal : positions ary;
}

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
  let rows = inst.rows in
  let goal = inst.goal in
  let size = rows * inst.cols in
  let tab = Array.make_matrix size size 0 in
  for tile = 1 to size - 1 do
    let locs = tab.(tile) in
    for loc = 0 to size - 1 do
      let srow = loc / rows and scol = loc mod rows in
      let dloc = get goal tile in
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
      let tile = get contents pos in
      get goal tile = pos && in_pos contents (pos + 1)
    end else
      true in
  (fun state -> in_pos state.contents 0)
