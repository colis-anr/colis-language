open Constraints_common
open Core

let fpf = Format.fprintf

let pp fmt c =
  fpf fmt "@[<h 2>globals:@\n";
  IVar.iter_globals
    c.globals
    (fun gx x ->
       fpf fmt "%a -> %a@\n"
         Var.pp gx IVar.pp x);
  fpf fmt "@]";
  IVar.iter
    c.info
    (fun x info ->
       fpf fmt "@[<h 2>%a:@\n" IVar.pp x;
       fpf fmt "nfeats: %a@\n"
         (Format.pp_print_list
            ~pp_sep:(fun fmt () -> fpf fmt ", ")
            (fun fmt (f, y) -> fpf fmt "¬⋅[%a]%a" Feat.pp f IVar.pp y))
         info.nfeats;
       fpf fmt "nabs: %a@\n"
         (Format.pp_print_list
            ~pp_sep:(fun fmt () -> fpf fmt ", ")
            (fun fmt f -> fpf fmt "¬⋅[%a]↑" Feat.pp f))
         info.nabs;
       fpf fmt "nfens: %a@\n"
         (Format.pp_print_list
            ~pp_sep:(fun fmt () -> fpf fmt ", ")
            (fun fmt fs -> fpf fmt "¬⋅[%a]" Feat.Set.pp fs))
         info.nfens;
       fpf fmt "nsims: %a@\n"
         (Format.pp_print_list
            ~pp_sep:(fun fmt () -> fpf fmt ", ")
            (fun fmt (fs, y) -> fpf fmt "¬⋅~%a%a" Feat.Set.pp fs IVar.pp y))
         info.nsims;
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
           fpf fmt "fen: %b@\n" d.fen;
           fpf fmt "sims: %a@\n"
             (Format.pp_print_list
                ~pp_sep:(fun fmt () -> fpf fmt ", ")
                (fun fmt (fs, y) -> fpf fmt "⋅~%a%a" Feat.Set.pp fs IVar.pp y))
             d.sims;
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
       );
       fpf fmt "@]@\n@?")
