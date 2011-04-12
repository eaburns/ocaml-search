(** Grid path-finding instances. *)

open Scanf
open Printf

type moves = Four_way | Eight_way

type cost = Unit

type inst = {
  w : int;
  h : int;
  blkd : bool array;
  moves : moves;
  costs : cost;
  x0 : int;
  y0 : int;
  xg : int;
  yg : int;
}

let id h = fun x y -> x * h + y

let loc h = fun id -> id / h, id mod h

let read_blkd ch w h blkd =
  let id = id h in
  for y = h - 1 downto 0 do
    for x = 0 to w - 1 do
      match input_char ch with
	| '#' -> blkd.(id x y) <- true
	| ' ' -> ()
	| c -> failwith (sprintf "Grid_inst.read_blkd: bad character: '%c'" c)
    done;
    let c = input_char ch in
    if c <> '\n' then failwith "Grid_inst.read_blkd: expected a newline";
  done

let read ch =
  let w, h = sscanf (input_line ch) " %d %d" (fun w h -> w, h) in
  ignore (input_line ch);
  let blkd = Array.create (w * h) false in
  read_blkd ch w h blkd;
  let costs = match input_line ch with
    | "Unit" -> Unit
    | c -> failwith (sprintf "Grid_inst.read: unknown costs: '%s'" c) in
  let moves = match input_line ch with
    | "Four-way" -> Four_way
    | "Eight-way" -> Eight_way
    | m -> failwith (sprintf "Grid_inst.read: unknown moves: '%s'" m) in
  let x0, y0, xg, yg =
    sscanf (input_line ch) " %d %d %d %d"
      (fun x0 y0 xg yg -> x0, y0, xg, yg) in
  { w = w; h = h; blkd = blkd; moves = moves; costs = costs;
    x0 = x0; y0 = y0; xg = xg; yg = yg; }
