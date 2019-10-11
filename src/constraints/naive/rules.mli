open Colis_constraints_common

val accessibility : Literal.Set.t -> (Var.t * Var.Set.t) list
val replace_in_literal_set : var:Var.t -> by:Var.t -> Literal.Set.t -> Literal.Set.t

val all : (string * (Conj.t -> Disj.t option)) list
