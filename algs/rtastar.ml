module type Eval = sig
  type state
  (** [eval stop depth_limit state] returns a path to a goal if there
      is one found otherwise it returns an empty path and the float
      will be the minimum f value of the fringe nodes.  If a goal path
      is returned then the value of the float is undefined. *)
  val eval : (Info.t -> bool) -> Info.t -> int -> state -> state list * float
end

let minf (a:float) b = if a < b then a else b
let comparef (a:float) b = if a = b then 0 else if a < b then ~-1 else 1

(* Perform DFS using inplace modification. *)
module Inplace_eval(D :
  sig
    include Search.Domain
    include Search.Inplace with type inplace_state = state
  end) : Eval with type state = D.state =
struct
  type state = D.state

  let eval _stop _info _dlim _state = [], nan
end

(* Perform DFS using non-inplace methods. *)
module Outplace_eval(D : sig include Search.Domain end) :
  Eval with type state = D.state =
struct
  type state = D.state

  let eval _stop _info _dlim _state = [], nan
end

(* The core of the RTA* search algorithm. *)
module Make(E : Eval) (D : Search.Domain with type state = E.state) :
  Search.Alg with type state = D.state =
struct
  type state = D.state
  let search _info _lims _args _init =
    None
end

(* The inplace RTA* search module. *)
module Inplace (D :
  sig
    include Search.Domain
    include Search.Inplace with type inplace_state = state
  end) = Make(Inplace_eval(D))(D)

(* The out-of-place (normal?) RTA* search module. *)
module Outplace (D : Search.Domain) = Make(Outplace_eval(D))(D)
