open Constraints_common
let fpf = Format.fprintf

let pp = Core.pp_debug

let pp_as_dot ~name fmt c =
  let c = Core.simplify c in
  let fresh = let c = ref 0 in fun () -> incr c; "fresh_" ^ string_of_int !c in
  let pp_node fmt ?text x =
    Format.kasprintf
      (fun cont ->
         let text =
           match text with
           | None -> ""
           | Some text -> "<TR><TD>" ^ text ^ "</TD></TR>"
         in
         if text <> "" || cont <> "" then
           fpf fmt "%d [label=< <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">%s%s</TABLE> >];@\n"
             (Core.hash x) text cont
         else
           fpf fmt "%d [shape=point];@\n" (Core.hash x))
      "%a"
  in
  let pp_flat_edge fmt x y = (* assumes x and y are hashed *)
    (* let x = Core.hash x in
    let y = Core.hash y in *)
    let (x, y) = if x < y then (x, y) else (y, x) in
    fpf fmt "{ rank=same; %d -> %d [style=dotted,arrowhead=none,label=\"%a\"]; }@\n" x y
  in
  let pp_kind_and_fen fmt info =
    (match Core.get_kind info with
     | Any -> ()
     | Neg kinds ->
       fpf fmt "<TR>";
       List.iter (fpf fmt "<TD>¬%a</TD>" Kind.pp) kinds;
       fpf fmt "</TR>"
     | Pos kind ->
       fpf fmt "<TR><TD>%a</TD></TR>" Kind.pp kind);
    if Core.has_fen info then
      fpf fmt "<TR><TD>full</TD></TR>"
  in
  let pp_feats fmt x info_x =
    Core.iter_feats
      (fun f -> function
         | Core.DontKnow -> () (* FIXME *)
         | Absent ->
           let y = fresh () in
           fpf fmt "%s [label=\"⊥\"];@\n" y;
           fpf fmt "%d -> %s [style=dotted,label=< <S>%a</S> >];@\n" (Core.hash x) y Feat.pp f
         | Present y ->
           (* only print arrows to non-initial variables. bc initial variables
              wont be printed at all *)
           if not (Core.is_initial y c) then
             fpf fmt "%d -> %d [label=\"%a\"];@\n" (Core.hash x) (Core.hash y) Feat.pp f
         | Maybe ys ->
           List.iter
             (fun y ->
                if not (Core.is_initial y c) then
                  fpf fmt "%d -> %d [style=dashed,label=< <I>%a</I>? >];@\n" (Core.hash x) (Core.hash y) Feat.pp f)
             ys
      )
      info_x
  in
  let pp_sims fmt x info_x =
    let x = Core.hash x in
    Core.iter_sims
      (fun fs y ->
         let y = Core.hash y in
         if x < y then (* only one variable prints this *)
           pp_flat_edge fmt x y (fun fmt -> fpf fmt "~%a" Feat.Set.pp) fs)
      info_x
  in
  let pp fmt c =
    Core.iter_infos
      (fun x info_x ->
         if not (Core.is_initial x c) then
           let text =
             match Core.externalise x c with
             | [] -> None
             | xs ->
               xs
               |> List.sort Var.compare
               |> List.map Var.to_string
               |> String.concat ", "
               |> (fun s -> Some s)
           in
           pp_node fmt ?text x
             pp_kind_and_fen info_x)
      c;
    fpf fmt "@\n";
    Core.iter_equalities
      (fun x y ->
         let text =
           match Core.externalise x c with
           | [] -> None
           | xs ->
             xs
             |> List.map Var.to_string
             |> String.concat ", "
             |> (fun s -> Some s)
         in
         pp_node fmt ?text x
           (fun _fmt () -> ()) ();
         pp_flat_edge fmt (Core.hash x) (Core.hash y) (fun fmt () -> fpf fmt "=") ())
      c;
    fpf fmt "@\n";
    Core.iter_infos (pp_feats fmt) c;
    fpf fmt "@\n";
    Core.iter_infos (pp_sims fmt) c
  in
  fpf fmt "@[<h 2>digraph %S {@\nnode [shape=plaintext];@\nedge [arrowhead=none];@\n@\n%a@]@\n}" name pp c
