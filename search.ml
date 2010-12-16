(** Signatures of searh modules.

    @author eaburns
    @since 2010-12-15
*)

module type Domain = sig
  (** The minimum domain interface. *)
  type state
  include Hashtbl.HashedType with type t = state
  val expand : state -> (state * float) list
  val is_goal : state -> bool
  val h : state -> float
  val d : state -> float
end


module type Alg = sig
  (** The minimum algorithm interface. *)
  type state
  val search : state -> (state list * float) option
end
