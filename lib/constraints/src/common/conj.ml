type t = Var.Set.t * Literal.Set.t
[@@deriving eq, ord]

let pp fmt (es, c) =
  let fpf = Format.fprintf in
  (
    match Var.Set.elements es with
    | [] -> ()
    | v :: vs ->
       fpf fmt "∃ %a" Var.pp v;
       List.iter (fpf fmt ", %a" Var.pp) vs
  );
  (
    match Literal.Set.elements c with
    | [] ->
       fpf fmt "⊤"
    | l :: ls ->
       Literal.pp fmt l;
       List.iter (fpf fmt " ∧ %a" Literal.pp) ls
  )
