module type Eval = sig
  type state
  val eval : float -> state -> float
end

(* Perform DFS using inplace modification. *)
module Inplace_eval(D :
  sig
    include Search.Domain
    include Search.Inplace with type inplace_state = state
  end) : Eval with type state = D.state =
struct
  type state = D.state

  let eval _ _ = nan
end

(* Perform DFS using non-inplace methods. *)
module Outplace_eval(D : sig include Search.Domain end) :
  Eval with type state = D.state =
struct
  type state = D.state

  let eval _ _ = nan
end

(* The core of the RTA* search algorithm. *)
module Make(E : Eval) (D : Search.Domain with type state = E.state) :
  Search.Alg with type state = D.state =
struct
  type state = D.state
  let search _info _limit _args _init =
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
