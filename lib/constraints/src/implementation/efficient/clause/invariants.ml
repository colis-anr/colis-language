(* This module is here to provide runtime checks of invariants that cannot be
   ensured syntactically by the structure. *)
(* FIXME: ppx_debug *)
open Constraints_common
open Core

exception Violated of string
let violated ?(if_=true) s =
  if if_ then raise (Violated s)

let rec iter_pairs f = function
  | [] -> ()
  | h :: t ->
    List.iter (fun h' -> f h h') t;
    iter_pairs f t

let check_kind info =
  (* There cannot be twice the same negative kind. Negative kinds are sorted.
     There cannot be all the negative kinds at the same time, nor all but one.
     The positive Dir kind is the Dir constructor and not Pos Dir. *)
  match info.kind with
  | Neg kinds ->
    iter_pairs (fun k1 k2 -> violated ~if_:(Kind.equal k1 k2) "kind.uniq") kinds;
    violated ~if_:(List.sort Kind.compare kinds <> kinds) "kind.sorted";
    violated ~if_:(List.length kinds = Kind.nb_all)       "kind.size";
    violated ~if_:(List.length kinds = Kind.nb_all - 1)   "kind.size"
  | Pos Kind.Dir -> violated "kind"
  | _ -> ()

let check_nfeats equalities info =
  (* No nfeats when not directory. Never twice the same nfeat. *)
  iter_pairs
    (fun (f, y) (g, z) ->
       violated
         ~if_:(Feat.equal f g && IVar.equal equalities y z)
         "nfeats.uniq")
    info.nfeats;
  match info.kind with
  | Pos _ -> violated ~if_:(info.nfeats <> []) "nfeats.not-dir"
  | Neg kinds when List.mem Kind.Dir kinds ->
    violated ~if_:(info.nfeats <> []) "nfeats.not-dir"
  | _ -> ()

let check_nabs info =
  (* No nabs when directory (because we moved the information to an other
     place). No nabs when not directory because subsumed. Never twice the same
     nabs. *)
  iter_pairs
    (fun f g ->
       violated
         ~if_:(Feat.equal f g)
         "nabs.uniq")
    info.nabs;
  match info.kind with
  | Dir _ -> violated ~if_:(info.nabs <> []) "nabs.dir"
  | Pos _ -> violated ~if_:(info.nabs <> []) "nabs.not-dir"
  | Neg kinds when List.mem Kind.Dir kinds ->
    violated ~if_:(info.nabs <> []) "nabs.not-dir"
  | _ -> ()

let check_nfens info =
  (* If we have ¬x[F] and ¬x[G], then there is no inclusion between F and G.
     No nfens if fen. No nfens if not directory. *)
  iter_pairs
    (fun fs gs ->
       violated ~if_:(Feat.Set.subset fs gs) "nfens.incl";
       violated ~if_:(Feat.Set.subset gs fs) "nfens.incl")
    info.nfens;
  match info.kind with
  | Dir { fen = true ; _ } ->
    violated ~if_:(info.nfens <> []) "nfens.fen"
  | Pos _ ->
    violated ~if_:(info.nfens <> []) "nfens.not-dir"
  | Neg kinds when List.mem Kind.Dir kinds ->
    violated ~if_:(info.nfens <> []) "nfens.not-dir"
  | _ -> ()

let check_nsims clause info =
  (* If we have x ~/F y and x ~/G y, then there is no inclusion between F and G.
     No nsims if fen. No nsims if not directory. *)
  iter_pairs
    (fun (fs, y) (gs, z) ->
       if IVar.equal clause.info y z then
         (violated ~if_:(Feat.Set.subset fs gs) "nsims.incl";
          violated ~if_:(Feat.Set.subset gs fs) "nsims.incl"))
    info.nsims;
  match info.kind with
  | Dir { fen = true ; _ } ->
    violated ~if_:(info.nsims <> []) "nsims.fen"
  | Pos _ ->
    violated ~if_:(info.nsims <> []) "nsims.not-dir"
  | Neg kinds when List.mem Kind.Dir kinds ->
    violated ~if_:(info.nsims <> []) "nsims.not-dir"
  | _ -> ()

let check_sims_refl clause =
  (* For each x ~F y, we have a y ~F x. *)
  IVar.iter
    clause.info
    (fun x info ->
       match info.kind with
       | Dir dir ->
         List.iter
           (fun (fs, y) ->
              ignore x;
              ignore y;
              ignore fs;
              (* FIXME: get infos for y and check that it is a dir with a
                 similarity with x with the same indice. *)
              ()
           )
           dir.sims
       | _ -> ())

let check_nsims_refl _clause = assert false

let check_info clause info =
  check_kind info;
  check_nfeats clause.info info;
  check_nabs info;
  check_nfens info;
  check_nsims clause info;
  check_sims_refl clause;
  check_nsims_refl clause

let check clause =
  IVar.iter clause.info (fun _ -> check_info clause)
