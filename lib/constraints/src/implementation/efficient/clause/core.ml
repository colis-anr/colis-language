open Constraints_common

type kind =
  | Any
  | Neg of Kind.t list
  | Pos of Kind.t

type feat =
  | DontKnow
  | Absent
  | Present of IVar.t
  | Maybe of IVar.t list

type info = {
  initial : bool ;
  kind : kind ;
  feats : feat Feat.Map.t ;
  fen : bool ;                         (* => dir *)
  sims : (Feat.Set.t * IVar.t) list ;  (* => dir *)
  nfeats : (Feat.t * IVar.t) list ;    (* => ¬ dir *)
  nabs : Feat.t list ;                 (* => ¬ dir *)
  nfens : Feat.Set.t list ;            (* => ¬ dir *)
  nsims : (Feat.Set.t * IVar.t) list ; (* => ¬ dir *)
}

type t =
  { globals : IVar.globals ;
    info : info IVar.map }

let empty = {
  globals = IVar.empty_globals ;
  info = IVar.empty_map
}

let empty_info = {
  initial = false ;
  kind = Any ;
  feats = Feat.Map.empty ;
  fen = false ;
  sims = [] ;
  nfeats = [] ;
  nabs = [] ;
  nfens = [] ;
  nsims = [] ;
}

let get_info x c =
  let info = IVar.get c.info x in
  { info with initial = false }

let set_info x c i =
  [{ c with info = IVar.set c.info x i }]

let update_info x c f =
  let info = IVar.get c.info x in
  f { info with initial = false }
  |> List.map
    (fun info ->
       { c with info = IVar.set c.info x info })
