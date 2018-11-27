open Constraints_common

type t = Var.Set.t * Literal.Set.t
[@@deriving eq, ord]

let pp fmt (es, c) =
  if not (Var.Set.is_empty es) then
    Format.fprintf fmt "∃ %a⋅ " Var.Set.pp es;
  if Literal.Set.is_empty c then
    Format.pp_print_string fmt "⊤"
  else
    Literal.Set.pp fmt c

type disj = t list
[@@deriving show { with_path = false }]

let pp_disj fmt = function
  | [] -> Format.pp_print_string fmt "⊥"
  | [conj] -> pp fmt conj
  | disj -> pp_disj fmt disj
