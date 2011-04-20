(* Information on a search. *)

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
  Printf.fprintf ch "expanded: %d\n" i.expd;
  Printf.fprintf ch "generated: %d\n" i.gend;
  Printf.fprintf ch "duplicates: %d\n" i.dups
