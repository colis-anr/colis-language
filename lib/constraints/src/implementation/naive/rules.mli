open Constraints_common

val accessibility : Literal.Set.t -> (Var.t * Var.Set.t) list
val replace_var_in_literal_set : Var.t -> Var.t -> Literal.Set.t -> Literal.Set.t

val all : (string * (Conj.t -> Disj.t option)) list
