(* Search limits. *)

open Printf

type t =
  | Expd of int
  | Gend of int
  | Time of float

(** Builds a function that test if the given limit has been
    reached. *)
let make_reached lims =
  let t = Sys.time () in
  let final_times t = function Time lim -> Time (t +. lim) | l -> l in
  let lims = List.map (final_times t) lims in
  let rec limit_reached info = function
    | Expd n :: lims -> info.Info.expd >= n || limit_reached info lims
    | Gend n :: lims -> info.Info.gend >= n || limit_reached info lims
    | Time lim :: lims -> lim <= Sys.time () || limit_reached info lims
    | [] -> false in
  (fun info -> limit_reached info lims)

(* Builds an [Arg.spec list].

   @param lims is a [t list ref] that the limits will be added to if
   parsed from the argument list.
*)
let arg_spec lims =
  [ "--expd",
    Arg.Int (fun l -> lims := Expd l :: !lims),
    "Set a node expansion limit";

    "--gend",
    Arg.Int (fun l -> lims := Gend l :: !lims),
    "Set a node generation limit";

    "--time",
    Arg.Float (fun l -> lims := Time l :: !lims),
    "Set a CPU time limit"; ]

let output ch lims =
  let rec out = function
    | Expd n :: ls ->
      fprintf ch "#pair  \"expansion limit\" \"%d\"\n" n;
      out ls
    | Gend n :: ls ->
      fprintf ch "#pair  \"generation limit\" \"%d\"\n" n;
      out ls
    | Time t :: ls ->
      fprintf ch "#pair  \"time limit\" \"%g\"\n" t;
      out ls
    | [] -> ()
  in out lims
