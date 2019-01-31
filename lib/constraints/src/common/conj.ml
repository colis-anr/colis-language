type t = Var.Set.t * Literal.Set.t
[@@deriving eq, ord]

let fpf = Format.fprintf

let pp fmt (es, c) =
  (
    match Var.Set.elements es with
    | [] -> ()
    | v :: vs ->
       fpf fmt "∃ %a" Var.pp v;
       List.iter (fpf fmt ", %a" Var.pp) vs;
       fpf fmt "⋅ "
  );
  (
    match Literal.Set.elements c with
    | [] ->
       fpf fmt "⊤"
    | l :: ls ->
       Literal.pp fmt l;
       List.iter (fpf fmt " ∧ %a" Literal.pp) ls
  )

let vars (es, c) =
  let us = Literal.Set.fold (fun l xs -> Var.Set.union (Literal.vars l) xs) c Var.Set.empty in
  Var.Set.union es us

let pp_as_dot ~name fmt (es, c) =
  let pp_literal_no_var fmt = function
    | Literal.Pos (Eq _) -> fpf fmt "="
    | Neg (Eq _) -> fpf fmt "≠"
    | Pos (Abs (_, f)) -> fpf fmt "[%a]↑" Feat.pp f
    | Pos (Kind (_, k)) -> Kind.pp fmt k
    | Neg (Kind (_, k)) -> fpf fmt "¬%a" Kind.pp k
    | Pos (Fen (_, fs)) -> fpf fmt "[%a]" (Feat.Set.pp_gen ~open_:"" ~close:"" ~empty:"") fs
    | Neg (Fen (_, fs)) -> fpf fmt "¬[%a]" (Feat.Set.pp_gen ~open_:"" ~close:"" ~empty:"") fs
    | Pos (Sim (_, fs, _)) -> fpf fmt "∼%a" (Feat.Set.pp_gen ~open_:"\\{" ~close:"\\}" ~empty:"∅") fs
    | Neg (Sim (_, fs, _)) -> fpf fmt "≁%a" (Feat.Set.pp_gen ~open_:"\\{" ~close:"\\}" ~empty:"∅") fs
    | _ -> failwith "pp_literal_no_var"
  in

  let is_atom_unary_about_var x = function
    | Atom.Eq _ | Feat _ | Sim _ -> false
    | Abs (y, _) | Kind (y, _) | Fen (y, _) -> Var.equal x y
  in

  let is_literal_unary_about_var x = function
    | Literal.Pos a | Neg a -> is_atom_unary_about_var x a
  in

  let pp_var_unary_attributes fmt x =
    Literal.Set.iter
      (fun l ->
        if is_literal_unary_about_var x l then
          fpf fmt " | %a" pp_literal_no_var l)
      c
  in

  let hash = Hashtbl.hash in

  fpf fmt "@[<h 2>digraph %S {@\n" name;

  (* Print nodes and unary literals. *)
  Var.Set.iter
    (fun x ->
      fpf fmt "%d [shape=rectangle,label=\"%s%a%a\"];@\n"
        (hash x)
        (if Var.Set.mem x es then "∃" else "")
        Var.pp x pp_var_unary_attributes x)
    (vars (es, c));

  (* Print binary literals *)
  Literal.Set.iter
    (function
     | Pos (Feat (x, f, y)) ->
        fpf fmt "%d -> %d [label=\"%a\"];@\n" (hash x) (hash y) Feat.pp f
     | Neg (Feat (x, f, y)) ->
        fpf fmt "%d -> %d [label=\"¬%a\", style=dashed, constraint=false];@\n"
          (hash x) (hash y) Feat.pp f
     | Pos (Sim (x, fs, y)) ->
        fpf fmt "%d -> %d [label=\"∼%a\", style=dashed, arrowhead=none, constraint=false];@\n"
          (hash x) (hash y) Feat.Set.pp fs
     | Neg (Sim (x, fs, y)) ->
        fpf fmt "%d -> %d [label=\"≁%a\", style=dashed, arrowhead=none, constraint=false];@\n"
          (hash x) (hash y) Feat.Set.pp fs
     | _ ->
        ())
    c;
  fpf fmt "}@]"
