(** Signatures of searh modules.

    @author eaburns
    @since 2010-12-15
*)

(** {1 Search Domains} *)

(** The minimum domain interface.  States are hashable. *)
module type Domain = sig
  type state
  include Hashtbl.HashedType with type t = state
  val succs : state -> (state * float) list
  val is_goal : state -> bool
  val h : state -> float
  val d : state -> int
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
  val cost : metric_state -> metric_state -> float
  val dist : metric_state -> metric_state -> int
end

(** Domains where it is possible to compute the predecessors of a
    state. *)
module type Reversable = sig
  type revable_state
  val preds : revable_state -> (revable_state * float) list
end

(** Domains where it is possible to compute the predecessors of a
    state and with named operators for generating states. *)
module type Reversable_with_ops = sig
  type rev_ops_state
  module Operator : Hashtbl.HashedType
  val succs_ops : rev_ops_state -> (rev_ops_state * Operator.t * float) list
  val preds_ops : rev_ops_state -> (rev_ops_state * Operator.t * float) list
end

(** {1 Search algorithms} *)

(** A search algorithm interface. *)
module type Alg = sig
  type state
  val search : state -> (state list * float) option
end
