(** Builds a table of algorithms for a given domain and allows lookup
    by strings Builds a table of algorithms for a given domain and allows
    lookup by strings.

    @author eaburns
    @since 2010-12-15
*)

module Make (D : Search.Domain) = struct
  let lookup = function
    | "bfs" -> let module M = Breadth_first.Make(D) in M.search
    | a -> failwith ("Unknown algorithm: " ^ a)
end
