open Colis_constraints_common
let fpf = Format.fprintf

let pp = Core.pp_debug

module Dot = struct
  let fresh = let c = ref 0 in fun () -> incr c; "fresh_" ^ string_of_int !c

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
           fpf fmt "%d [label=< <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\"><TR><TD></TD></TR></TABLE> >];@\n"
             (Core.hash x))
      "%a"

  let pp_flat_edge fmt x y = (* assumes x and y are hashed *)
    (* let x = Core.hash x in
       let y = Core.hash y in *)
    let (x, y) = if x < y then (x, y) else (y, x) in
    fpf fmt "{ rank=same; %d -> %d [style=dotted,arrowhead=none,label=\"%a\"]; }@\n" x y
  let pp_kind_and_fen fmt info =
    (match Core.Info.get_kind info with
     | Any -> ()
     | Neg kinds ->
       fpf fmt "<TR>";
       List.iter (fpf fmt "<TD>¬%a</TD>" Kind.pp) kinds;
       fpf fmt "</TR>"
     | Pos kind ->
       fpf fmt "<TR><TD>%a</TD></TR>" Kind.pp kind);
    if Core.Info.has_fen info then
      fpf fmt "<TR><TD>full</TD></TR>"

  let pp_feats fmt shadows x info_x =
    if not (Core.VarSet.mem x shadows) then
      Core.Info.iter_feats
        (fun f -> function
           | Core.Info.DontKnow -> () (* FIXME *)
           | Absent ->
             let y = fresh () in
             fpf fmt "%s [label=\"⊥\"];@\n" y;
             fpf fmt "%d -> %s [style=dotted,label=< <S>%a</S> >];@\n" (Core.hash x) y Feat.pp f
           | Present y ->
             (* only print arrows to non-initial variables. bc initial variables
                wont be printed at all *)
             if not (Core.VarSet.mem y shadows) then
               fpf fmt "%d -> %d [label=\"%a\"];@\n" (Core.hash x) (Core.hash y) Feat.pp f
           | Maybe ys ->
             List.iter
               (fun y ->
                  if not (Core.VarSet.mem y shadows) then
                    fpf fmt "%d -> %d [style=dashed,label=< <I>%a</I>? >];@\n" (Core.hash x) (Core.hash y) Feat.pp f)
               ys
        )
        info_x

  let pp_sims fmt shadows x info_x =
    let hx = Core.hash x in
    Core.Info.iter_sims
      (fun fs y ->
         let hy = Core.hash y in
         (* only one variable prints this *)
         if hx < hy && not (Core.VarSet.mem x shadows) && not (Core.VarSet.mem y shadows) then
           pp_flat_edge fmt hx hy (fun fmt -> fpf fmt "~%a" Feat.Set.pp) fs)
      info_x

  let compute_shadows c =
    (* Each node has a 'shadow' flag in the constraint. However, if we just
       consider this flag in the printing, it gives weird results. This is
       because there can be nodes that are non-shadow with parents that are.
       This function computes a set of non-shadow nodes by taking (a) all the
       nodes that are flagged as non-shadow, and (b) all the nodes that are
       parents from a non-shadow node. *)
    let rec handle_node x (seen, shadows) =
      if Core.VarSet.mem x seen then
        (* If the node has already been seen, no need to do anything. *)
        (seen, shadows)
      else
        (
          let info_x = Core.get_info_no_shadow x c in
          (* Firstly, mark the node [x] as seen. Moreover, if the node [x] has
             the shadow flag, then add it to the set of shadow nodes. We might
             later remove it from there. *)
          let seen = Core.VarSet.add x seen in
          let shadows =
            if Core.Info.is_shadow info_x then
              Core.VarSet.add x shadows
            else
              shadows
          in
          (* Secondly, iterate through all the children of [x], handling them
             all, then checking whether they are shadow or not. If they are not
             shadow, remove them from the [shadows] set. This is the job of
             [handle_node_and_mark_parent]. *)
          Core.Info.fold_feats
            (fun _ feat (seen, shadows) ->
               match feat with
               | DontKnow | Absent -> (seen, shadows)
               | Present y -> handle_node_and_mark_parent ~parent:x y (seen, shadows)
               | Maybe ys ->
                 List.fold_left (fun e y -> handle_node_and_mark_parent ~parent:x y e) (seen, shadows) ys)
            (seen, shadows)
            info_x
        )
    and handle_node_and_mark_parent ~parent y (seen, shadows) =
      (* This function handles the node [y]. Once this is done, we know whether
         it is a shadow node or not. If it is not, then we mark its parent [x]
         as non-shadow also. *)
      let (seen, shadows) = handle_node y (seen, shadows) in
      if Core.VarSet.mem y shadows then
        (seen, shadows)
      else
        (seen, Core.VarSet.remove parent shadows)
    in
    (Core.VarSet.empty, Core.VarSet.empty)
    |> Core.fold_infos (fun x _ e -> handle_node x e) c
    |> snd
    (* Remove all the globals from the shadows: *)
    |> Core.fold_globals Core.VarSet.remove c

  let pp ~name fmt c =
    let c = Core.simplify c in
    let shadows = compute_shadows c in
    let pp fmt c =
      Core.iter_infos
        (fun x info_x ->
           if not (Core.VarSet.mem x shadows) then
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
      Core.iter_infos (pp_feats fmt shadows) c;
      fpf fmt "@\n";
      Core.iter_infos (pp_sims fmt shadows) c
    in
    fpf fmt "@[<h 2>digraph %S {@\nnode [shape=plaintext];@\nedge [arrowhead=none];@\n@\n%a@]@\n}" name pp c
end

let pp_as_dot = Dot.pp
