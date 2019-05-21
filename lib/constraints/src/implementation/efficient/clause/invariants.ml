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
    iter_pairs (fun k1 k2 -> violated ~if_:(Kind.equal k1 k2) "kind") kinds;
    violated ~if_:(List.length kinds = Kind.nb_all)     "kind";
    violated ~if_:(List.length kinds = Kind.nb_all - 1) "kind"
  | Pos Kind.Dir -> violated "kind"
  | _ -> ()

let check_nfens info =
  (* If we have ¬x[F] and ¬x[G],
     then there is no inclusion between F and G. *)
  iter_pairs
    (fun fs gs ->
       violated ~if_:(Feat.Set.subset fs gs) "nfens";
       violated ~if_:(Feat.Set.subset gs fs) "nfens")
    info.nfens

let check_nsims clause info =
  (* If we have x ~/F y and x ~/G y,
     then there is no inclusion between F and G. *)
  iter_pairs
    (fun (fs, y) (gs, z) ->
       if IVar.equal clause.info y z then
         (violated ~if_:(Feat.Set.subset fs gs) "nsims";
          violated ~if_:(Feat.Set.subset gs fs) "nsims"))
    info.nsims

let check_nfens_fen info =
  (* We cannot have both a fen and nfens. *)
  match info.kind with
  | Dir { fen = true ; _ } ->
    violated ~if_:(info.nfens = []) "nfens_fen"
  | _ -> ()

let check_nsims_fen info =
  (* We cannot have both a fen and nsims. *)
  match info.kind with
  | Dir { fen = true ; _ } ->
    violated ~if_:(info.nsims = []) "nsims_fen"
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

let check_info clause info =
  check_kind info;
  check_nfens info;
  check_nsims clause info;
  check_nfens_fen info;
  check_nsims_fen info;
  check_sims_refl clause

let check clause =
  IVar.iter clause.info (fun _ -> check_info clause)
