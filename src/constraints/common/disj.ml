type t = Conj.t list
[@@deriving show { with_path = false }]

let pp fmt = function
  | [] -> Format.pp_print_string fmt "⊥"
  | [conj] -> Conj.pp fmt conj
  | disj -> pp fmt disj
