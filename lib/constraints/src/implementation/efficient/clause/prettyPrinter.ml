open Constraints_common
open Core

let fpf = Format.fprintf

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
           fpf fmt "kind: ";
           match info.kind with
           | Any -> fpf fmt "*@\n"
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

let pp_as_dot ~name _ _ = ignore name; assert false
