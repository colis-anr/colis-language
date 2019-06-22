open Constraints_common
open Core
let fpf = Format.fprintf

let make_initial c =
  { globals = c.globals ;
    info = IVar.map c.info (fun info -> { info with initial = true }) }

let pp fmt c =
  IVar.iter_globals
    c.globals
    (fun gx x ->
       fpf fmt "%a -> %a@\n"
         Var.pp gx IVar.pp x);
  fpf fmt "@\n";
  IVar.iter_sons
    c.info
    (fun x y ->
       fpf fmt "%a = %a@\n"
         IVar.pp x IVar.pp y);
  fpf fmt "@\n";
  IVar.iter
    c.info
    (fun x info ->
       fpf fmt "@[<h 2>%a:@\n" IVar.pp x;
       if info.kind <> Any then
         (
           if info.initial then
             fpf fmt "initial@\n";
           fpf fmt "kind: ";
           match info.kind with
           | Any -> assert false
           | Neg kinds ->
             Format.pp_print_list ~pp_sep:(fun fmt () -> fpf fmt ", ") (fun fmt k -> fpf fmt "¬%a" Kind.pp k) fmt kinds;
             fpf fmt "@\n"
           | Pos kind -> fpf fmt "%a@\n" Kind.pp kind
           | Dir d ->
             fpf fmt "dir@\n";
             if d.fen then
               fpf fmt "fen: %b@\n" d.fen;
             if d.sims <> [] then
               fpf fmt "sims: %a@\n"
                 (Format.pp_print_list
                    ~pp_sep:(fun fmt () -> fpf fmt ", ")
                    (fun fmt (fs, y) -> fpf fmt "⋅~%a%a" Feat.Set.pp fs IVar.pp y))
                 d.sims;
             if d.feats <> Feat.Map.empty then
               (
                 fpf fmt "@[<h 2>feats:@\n";
                 Feat.Map.iter
                   (fun f t ->
                      fpf fmt "%a -> " Feat.pp f;
                      match t with
                      | DontKnow -> fpf fmt "?@\n"
                      | Exists -> fpf fmt "exists@\n"
                      | Pointsto y -> fpf fmt "%a@\n" IVar.pp y
                      | Noresolve _ -> fpf fmt "noresolve (FIXME)@\n")
                   d.feats;
                 fpf fmt "@]"
               )
         );
       if info.nfeats <> [] then
         fpf fmt "nfeats: %a@\n"
           (Format.pp_print_list
              ~pp_sep:(fun fmt () -> fpf fmt ", ")
              (fun fmt (f, y) -> fpf fmt "¬⋅[%a]%a" Feat.pp f IVar.pp y))
           info.nfeats;
       if info.nabs <> [] then
         fpf fmt "nabs: %a@\n"
           (Format.pp_print_list
              ~pp_sep:(fun fmt () -> fpf fmt ", ")
              (fun fmt f -> fpf fmt "¬⋅[%a]↑" Feat.pp f))
           info.nabs;
       if info.nfens <> [] then
         fpf fmt "nfens: %a@\n"
           (Format.pp_print_list
              ~pp_sep:(fun fmt () -> fpf fmt ", ")
              (fun fmt fs -> fpf fmt "¬⋅[%a]" Feat.Set.pp fs))
           info.nfens;
       if info.nsims <> [] then
         fpf fmt "nsims: %a@\n"
           (Format.pp_print_list
              ~pp_sep:(fun fmt () -> fpf fmt ", ")
              (fun fmt (fs, y) -> fpf fmt "¬⋅~%a%a" Feat.Set.pp fs IVar.pp y))
           info.nsims;
       fpf fmt "@]@\n@?")

let pp_as_dot ~name fmt c =
  let fresh = let c = ref 0 in fun () -> incr c; "fresh_" ^ string_of_int !c in
  let pp_node fmt x s =
    fpf fmt "%a [label=< <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\"><TR><TD COLSPAN=\"2\">%s</TD></TR>%a</TABLE> >];@\n"
      IVar.pp x s
  in
  let pp_flat_edge fmt x y =
    let (x, y) = if IVar.lt x y then (x, y) else (y, x) in
    fpf fmt "{ rank=same; %a -> %a [style=dotted,arrowhead=none,label=\"%a\"]; }@\n" IVar.pp x IVar.pp y
  in
  let pp_kind_and_fen fmt info =
    if info.kind <> Any then
      (
        match info.kind with
        | Any -> assert false
        | Neg kinds ->
          fpf fmt "<TR>";
          List.iter (fpf fmt "<TD>¬%a</TD>" Kind.pp) kinds;
          fpf fmt "</TR>"
        | Pos kind ->
          fpf fmt "<TR><TD>%a</TD></TR>" Kind.pp kind
        | Dir d -> fpf fmt "<TR><TD>dir</TD>%s</TR>" (if d.fen then "<TD>fen</TD>" else "");
      )
  in
  let pp_feats fmt x info_x =
    match info_x.kind with
    | Dir d ->
      Feat.Map.iter
        (fun f -> function
           | DontKnow -> () (* FIXME *)
           | Exists -> () (* FIXME *)
           | Pointsto y ->
             (* only print arrows to non-initial variables. bc initial variables
                wont be printed at all *)
             if not (IVar.get c.info y).initial then
               fpf fmt "%a -> %a [label=\"%a\"];@\n" IVar.pp x IVar.pp y Feat.pp f
           | Noresolve (C []) ->
             let y = fresh () in
             fpf fmt "%s [shape=point,style=invis,label=\"\"];@\n" y;
             fpf fmt "%a -> %s [arrowhead=box,label=\"%a\"];@\n" IVar.pp x y Feat.pp f
           | Noresolve _ -> () (* FIXME *)
        )
        d.feats
    | _ -> ()
  in
  let pp_sims fmt x info_x =
    match info_x.kind with
    | Dir d ->
      List.iter
        (fun (fs, y) ->
           if IVar.lt x y then (* only one variable prints this *)
             pp_flat_edge fmt x y (fun fmt -> fpf fmt "~%a" Feat.Set.pp) fs)
        d.sims
    | _ -> ()
  in
  (* FIXME: nfeats; nabs; nfens; nsims *)
  let pp fmt c =
    IVar.iter
      c.info
      (fun x info_x ->
         (* only print non-initial variables *)
         if not info_x.initial then
           pp_node fmt x
             (match IVar.get_global c.globals x with None -> "∃" | Some gx -> Constraints_common.Var.to_string gx)
             pp_kind_and_fen info_x);
    IVar.iter_sons
      c.info
      (fun x y ->
         pp_node fmt x
           (match IVar.get_global c.globals x with None -> "∃" | Some gx -> Constraints_common.Var.to_string gx)
           (fun _fmt () -> ()) ();
         pp_flat_edge fmt x y (fun fmt () -> fpf fmt "=") ());
    IVar.iter c.info (pp_feats fmt);
    IVar.iter c.info (pp_sims fmt)
  in
  fpf fmt "@[<h 2>digraph %S {@\nnode [shape=plaintext];@\n%a@]@\n}" name pp c
