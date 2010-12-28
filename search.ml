(** Signatures of searh modules.

    @author eaburns
    @since 2010-12-15
*)

module type Domain = sig
  (** The minimum domain interface.  States are hashable. *)
  type state
  include Hashtbl.HashedType with type t = state
  val succs : state -> (state * float) list
  val is_goal : state -> bool
  val h : state -> float
  val d : state -> int
end

module type Metric = sig
  type metric_state
  val cost : metric_state -> metric_state -> float
  val dist : metric_state -> metric_state -> int
end

module type Reversable = sig
  type revable_state
  val preds : revable_state -> (revable_state * float) list
end

module type Alg = sig
  (** The minimum algorithm interface. *)
  type state
  val search : state -> (state list * float) option
end
