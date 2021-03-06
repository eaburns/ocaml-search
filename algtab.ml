(** Builds a table of algorithms for a given domain and allows lookup
    by strings Builds a table of algorithms for a given domain and
    allows lookup by strings. *)

module Domain (D : Search.Domain) =
struct
  let table =
    [
      "bfs", (let module M = Breadth_first.Make(D) in M.search);
      "astar", (let module M = Astar.Make(D) in M.search);
(*
      "rtastar", (let module M = Rtastar.Outplace(D) in M.search);
      "idastar", (let module M = Idastar.Outplace(D) in M.search);
*)
    ]
end

module Inplace (D :
  sig
    include Search.Domain
    include Search.Inplace with type inplace_state = state
  end) =
struct
  let table =
    [
      "idastar-inplace", (let module M = Idastar.Inplace(D) in M.search);
(*
      "rtastar-inplace", (let module M = Rtastar.Inplace(D) in M.search);
*)
    ]
end

module Metric_domain (D :
  sig
    include Search.Domain
    include Search.Metric with type metric_state = state
  end) =
struct
  let table =
    [
    ]
end

module Reversable_domain (D :
  sig
    include Search.Domain
    include Search.Reversable with type revable_state = state
  end) =
struct
  let table =
    [
    ]
end

module Reversable_with_ops_domain (D :
  sig
    include Search.Domain
    include Search.Reversable_with_ops with type rev_ops_state = state
  end) =
struct
  let table =
    [
    ]
end
