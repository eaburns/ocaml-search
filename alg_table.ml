(** Builds a table of algorithms for a given domain and allows lookup
    by strings Builds a table of algorithms for a given domain and allows
    lookup by strings.

    NOTES: This is not quite right because it is just a table for
    algorithms that accept Search.Domain modules.  There would need to
    be multiple tables for different kinds of domains
    (Search.Std_domain, etc.).  It would then be the job of the
    domain's main function to concatinate the correct tables and
    perform a lookup.

    @author eaburns
    @since 2010-12-15
*)

module Make (D : Search.Domain) = struct
  let lookup = function
    | "bfs" -> let module M = Breadth_first.Make(D) in M.search
    | a -> failwith ("Unknown algorithm: " ^ a)
end
