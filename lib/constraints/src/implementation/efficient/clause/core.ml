open Constraints_common

type t =
  { globals : IVar.globals ;
    info : info IVar.map }

and info =
  { nfens : Feat.Set.t list ;
    nsims : (Feat.Set.t * IVar.t) list ;
    kind : kind }

and kind =
  | Any
  | Neg of Kind.t list
  | Pos of Kind.t
  | Dir of dir

and dir =
  { fen : bool ;
    sims : (Feat.Set.t * IVar.t) list ;
    feats : target Feat.Map.t }

and target =
  | Exists of IVar.t
  | Noresolve of feat_tree
(* Absence if a subcase of noresolve *)

and feat_tree = unit (* FIXME *)
