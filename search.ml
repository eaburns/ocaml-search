(** Signatures of search modules. *)

(** {1 Search Domains} *)


(** The minimum domain interface.  States are hashable. *)
module type Domain = sig
  type state
  include Hashtbl.HashedType with type t = state

  (** Gets the list of successor states and the operator cost that
      generated the state. *)
  val succs : parent:state -> state:state -> (state * float) list

  (** Tests if the given state is a goal. *)
  val is_goal : state -> bool

  (** Heuristic estimate of cost to goal. *)
  val h : state -> float

  (** Heuristic estimate of search effort to goal. *)
  val d : state -> int

  (** Print the state. *)
  val fmt : Format.formatter -> state -> unit
end

(** Search in a domain that allows in place modification of a single
    state. *)
module type Inplace = sig
  type inplace_state
  type iter
  type oper

  (* Get an iterator over the successors. *)
  val succ_iter : unit -> iter

  (* Gets the next child and the transition cost or None if there are
     no more children.  The current state is passed as part of the
     result so that the heuristic or goal test may be performed on
     it. *)
  val next : iter -> (inplace_state * float * oper) option

  (* Undo the given operation on the current state. *)
  val undo : oper -> unit

  (* Get a duplicate of the current state. *)
  val dup : unit -> inplace_state

end

(** Spaces that have types attached to their states. *)
module type Typed_state = sig
  type typed_state

  (** Module for dealing with state types.  This may be a bit more
      inefficient than just using integers but it may look cleaner in
      the domain code. *)
  module State_type : sig
    type t
    val to_int : typed_state -> int
  end

  val t : typed_state -> State_type.t
end

(** Spaces with a cost and distance function defined between arbitrary
    states. *)
module type Metric = sig
  type metric_state

  (** Heuristic estimate of cost between two different states. *)
  val cost : src:metric_state -> dst:metric_state -> float

  (** Heuristic estimate of search distance between two different
      states. *)
  val dist : src:metric_state -> dst:metric_state -> int
end

(** Domains where it is possible to compute the predecessors of a
    state. *)
module type Reversable = sig
  type revable_state

  (** The predecessor of a state and the operator cost that generates
      the given state from its predecessor. *)
  val preds : revable_state -> (revable_state * float) list
end

(** Domains where it is possible to compute the predecessors of a
    state and with named operators for generating states. *)
module type Reversable_with_ops = sig
  type rev_ops_state
  module Operator : Hashtbl.HashedType

  (** The successors of a state along with their generating operator
      and that operator's cost. *)
  val succs_ops : rev_ops_state -> (rev_ops_state * Operator.t * float) list

  (** The predecessors of a state along with the operator that would
      generate the given state from its predecessor and that
      operator's cost. *)
  val preds_ops : rev_ops_state -> (rev_ops_state * Operator.t * float) list
end

(** {1 Search algorithms} *)

(** A search algorithm interface. *)
module type Alg = sig
  type state
  val search :
    Info.t -> Limit.t list -> string array -> state
    -> (state list * float) option
end
