open Constraints_common

type t = Var.Set.t * Literal.Set.t
[@@deriving show { with_path = false }, eq, ord]

type disj = t list
[@@deriving show { with_path = false }]
