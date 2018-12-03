open Constraints_common

val accessibility : Literal.Set.t -> (Var.t * Var.Set.t) list

val all : (string * (Conj.t -> Disj.t option)) list
