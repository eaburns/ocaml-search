(** Builds a table of algorithms for a given domain and allows lookup
    by strings Builds a table of algorithms for a given domain and allows
    lookup by strings.

    @author eaburns
    @since 2010-12-15
*)

module Domain (D : Search.Domain) = struct
  let table =
    [
      "bfs", (let module M = Breadth_first.Make(D) in M.search);
      "astar", (let module M = Astar.Make(D) in M.search);
    ]
end
