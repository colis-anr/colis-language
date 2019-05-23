open Constraints_common

(* FIXME: neq *)

type t =
  { globals : IVar.globals ;
    info : info IVar.map }

and info =
  { nfeats : (Feat.t * IVar.t) list ;
    nabs : Feat.t list ;
    nfens : Feat.Set.t list ;
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
  | DontKnow
  | Exists
  | Pointsto of IVar.t
  | Noresolve of feat_tree
(* Absence if a subcase of noresolve *)

and feat_tree = C of (Feat.t * feat_tree) list

let empty =
  { globals = IVar.empty_globals ;
    info = IVar.empty_map }

let empty_dir =
  { fen = false ;
    sims = [] ;
    feats = Feat.Map.empty }

let get_info x c =
  IVar.get c.info x

let set_info x c i =
  IVar.set c.info x i

let update_info x c f =
  let info = IVar.get c.info x in
  f info
  |> List.map
    (fun info ->
       { c with info = IVar.set c.info x info })
