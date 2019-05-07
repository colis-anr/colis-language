open Constraints_common

module Type = struct
  type var = Var.t
  type equalities = (var * var) list

  type t =
    { globals : Var.Set.t ;
      equalities : equalities ;
      var_info : info Var.Map.t }

  and info =
    { nfens : Feat.Set.t list ;
      nsims : (Feat.Set.t * var) list ;
      kind : kind }

  and kind =
    | Any
    | NBlock | NChar | NPipe | NReg | NSock | NSymlink
    |  Block |  Char |  Pipe |  Reg |  Sock |  Symlink
    | NDir
    |  Dir of dir

  and dir =
    { fen : bool ;
      sims : (Feat.Set.t * var) list ;
      feats : maybe_feat Feat.Map.t }

  and maybe_feat =
    | Abs
    | Pres of var option
end

let are_var_equal equalities x y =
  Var.equal x y
  || List.exists (fun (a, b) ->
      (Var.equal x a && Var.equal y b)
      || (Var.equal y a && Var.equal x b))
    equalities

module Invariants = struct
  (* This module is here to provide runtime checks of invariants that cannot be
     ensured syntactically by the structure. *)
  (* FIXME: ppx_debug *)
  open Type

  exception Violated of string
  let violated s = raise (Violated s)
  let check_violated s b = if b then violated s

  let rec iter_pairs f = function
    | [] -> ()
    | h :: t ->
      List.iter (fun h' -> f h h') t;
      iter_pairs f t

  let check_nfens info =
    (* If we have ¬x[F] and ¬x[G],
       then there is no inclusion between F and G. *)
    iter_pairs
      (fun fs gs ->
         check_violated "nfens" (Feat.Set.subset fs gs);
         check_violated "nfens" (Feat.Set.subset gs fs))
      info.nfens

  let check_nsims clause info =
    (* If we have x ~/F y and x ~/G y,
       then there is no inclusion between F and G. *)
    iter_pairs
      (fun (fs, y) (gs, z) ->
         if are_var_equal clause.equalities y z then
           (check_violated "nsims" (Feat.Set.subset fs gs);
            check_violated "nsims" (Feat.Set.subset gs fs)))
      info.nsims

  let check_nfens_fen info =
    (* We cannot have both a fen and nfens. *)
    match info.kind with
    | Dir { fen = true ; _ } ->
      check_violated "nfens_fen" (info.nfens = [])
    | _ -> ()

  let check_nsims_fen info =
    (* We cannot have both a fen and nsims. *)
    match info.kind with
    | Dir { fen = true ; _ } ->
      check_violated "nsims_fen" (info.nsims = [])
    | _ -> ()

  let check_sims_refl clause =
    (* For each x ~F y, we have a y ~F x. *)
    Var.Map.iter
      (fun x info ->
         match info.kind with
         | Dir dir ->
           List.iter
             (fun (fs, y) ->
                (* FIXME: get infos for y and check that it is a dir with a
                   similarity with x with the same indice. *)
                ()
             )
             dir.sims
         | _ -> ())
      clause.var_info

  let check_info clause info =
    check_nfens info;
    check_nsims clause info;
    check_nfens_fen info;
    check_nsims_fen info;
    check_sims_refl clause

  let check clause =
    Var.Map.iter (fun _ -> check_info clause) clause.var_info
end
