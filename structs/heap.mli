(** A min heap implemented as a binary heap. *)

type 'a t

exception Empty

val no_pos : int
(** 'index' value given to elements that have been removed from the
    heap. *)

val init : ?index:('a -> int -> unit) -> ('a -> 'a -> bool) -> 'a array -> 'a t
(** [init ?index less elms] builds a heap from the given elements.

    @param less defines the order of the elements.

    @param index function is called whenever the index of an element in
    the heap changes. *)

val minimum : 'a t -> 'a

val update_key : 'a t -> int -> unit
(** This must be called when the key of the element at the given index
    has been updated to re-heap the element. *)

val pop : 'a t -> 'a

val push : 'a t -> 'a -> unit

val fill : 'a t -> int
(** Get the current fill. *)

val is_empty : 'a t -> bool

val exact_fit : 'a t -> unit
(** Truncates the excess capacity of the heap to the current fill. *)
