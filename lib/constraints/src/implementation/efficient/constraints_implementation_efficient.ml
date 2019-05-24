include Constraints_implementation_efficient_clause.External
include Constraints_implementation_efficient_clause.PrettyPrinter

(* let pp fmt _ = Format.fprintf fmt "<not implemented>" *)
let pp_as_dot ~name _ _ = ignore name; assert false
