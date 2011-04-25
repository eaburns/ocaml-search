open Scanf

(* Disallow any confusion between positions and contents arrays. *)
module Ary : sig
  type positions
  type contents
  type 'a ary = private string

  val positions : int -> positions ary
  val contents : int -> contents ary
  val set : 'a ary -> int -> int -> unit
  val get : 'a ary -> int -> int
  val copy : 'a ary -> 'a ary
  val equal : 'a ary -> 'a ary -> bool

end = struct
  type contents

  type positions

  type 'a ary = string

  let positions size = String.create size

  let contents size = String.create size

  let set str ind vl =
    str.[ind] <- Char.unsafe_chr vl

  let get str ind =
    Char.code str.[ind]

  let copy = String.copy

  let equal a b = String.compare a b = 0
end

include Ary

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

let read_positions ch size =
  let pos = positions size in
  for i = 0 to size - 1 do fscanf ch " %d" (fun p -> set pos i p) done;
  pos

let contents_of_positions : positions ary -> int -> contents ary =
  (fun pos size ->
    let conts = contents size in
    for tile = 0 to size - 1 do
      let p = get pos tile in
      set conts p tile
    done;
    conts)

let read ch =
  let rows, cols = fscanf ch "%d %d" (fun r c -> r, c) in
  let size = rows * cols in
  fscanf ch " %[^\n]" (fun _ -> ());
  let init = read_positions ch size in
  fscanf ch " %[^\n]" (fun _ -> ());
  let goal = read_positions ch size in
  { rows = rows;
    cols = cols;
    init = contents_of_positions init size;
    goal = goal; }
