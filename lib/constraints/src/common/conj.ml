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

let pp_as_dot ~name fmt (es, c) =
  let invis = let invis = ref 0 in fun () -> incr invis; !invis in

  fpf fmt "@[<h 2>digraph %s {@\n" name;

  (* Print nodes. Circle for existentially quantified, square for
     others. *)
  Var.Set.iter
    (fun v ->
      fpf fmt "\"%a\" [shape=circle];@\n" Var.pp v)
    es;
  fpf fmt "node [shape=square];@\n";

  (* Print literals. *)
  Literal.Set.iter
    (function
     | Pos (Feat (x, f, y)) ->
        fpf fmt "\"%a\" -> \"%a\" [label=\"%a\"];@\n" Var.pp x Var.pp y Feat.pp f
     | Pos (Abs (x, f)) ->
        let i = invis () in
        fpf fmt "\"%d\" [shape=point, style=invis];@\n" i;
        fpf fmt "\"%a\" -> \"%d\" [label=\"%a\", arrowhead=box];@\n" Var.pp x i Feat.pp f
     | Pos (Sim (x, fs, y)) ->
        fpf fmt "\"%a\" -> \"%a\" [label=\"~%a\", style=dashed, arrowhead=none, constraint=false];@\n" Var.pp x Var.pp y Feat.Set.pp fs
     | Neg (Sim (x, fs, y)) ->
        fpf fmt "\"%a\" -> \"%a\" [label=\"¬~%a\", style=dashed, arrowhead=none, constraint=false];@\n" Var.pp x Var.pp y Feat.Set.pp fs
     | l ->
        Log.err (fun m -> m "Conj.pp_as_dot: unsupported literal (%a)" Literal.pp l))
    c;
  fpf fmt "}@]"
