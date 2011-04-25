(* Information on a search. *)

open Printf

type t = {
  (* If all of these remain ints then there is no penalty for mutating
     them. *)
  mutable expd : int;
  mutable gend : int;
  mutable dups : int;
}

let create () =
  { expd = 0;
    gend = 0;
    dups = 0; }

let output ch i =
  fprintf ch "#pair  \"total nodes expanded\" \"%d\"\n" i.expd;
  fprintf ch "#pair  \"total nodes generated\" \"%d\"\n" i.gend;
  fprintf ch "#pair  \"total duplicates\" \"%d\"\n" i.dups
