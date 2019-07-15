open Constraints_common

exception NotImplemented of string
let not_implemented s = raise (NotImplemented s)

(** {2 Main Structure} *)

type var = int

type kind =
  | Any
  | Neg of Kind.t list (* uniques, sorted, less than (#kinds - 1) elements *)
  | Pos of Kind.t

type feat =
  | DontKnow
  | Absent
  | Present of var    (* implies dir *)
  | Maybe of var list

type info = {
  initial : bool ;
  kind : kind ;
  feats : feat Feat.Map.t ;
  fen : bool ;                      (* implies dir *)
  sims : (Feat.Set.t * var) list ;  (* max 1 for each variable, implies dir *)
  nfens : Feat.Set.t list ;         (* only if not "not dir" *)
  nsims : (Feat.Set.t * var) list ; (* only if not "not dir" *)
}

module IMap = Map.Make(struct type t = int let compare = compare end)
type info_or_son = Info of info | Son of var

type t = {
  (* Globals: not existentially quantified.
     Map from external variables to internal. *)
  globals : var Var.Map.t ;

  (* Info: a union-find carrying [info]. *)
  info : info_or_son IMap.t ;
}

let empty = {
  globals = Var.Map.empty ;
  info = IMap.empty
}

let empty_info = {
  initial = false ;
  kind = Any ;
  feats = Feat.Map.empty ;
  fen = false ;
  sims = [] ;
  nfens = [] ;
  nsims = [] ;
}

let fresh_var =
  let c = ref 0 in
  fun () -> incr c; !c

(** *)

let find_ancestor_and_info x c =
  let rec find_ancestor_and_info x =
    match IMap.find x c.info with
    | Info info -> (x, info)
    | Son y -> find_ancestor_and_info y
  in
  find_ancestor_and_info x

let equal x y c =
  let (ax, _) = find_ancestor_and_info x c in
  let (ay, _) = find_ancestor_and_info y c in
  ax = ay

let internalise x c =
  match Var.Map.find_opt x c.globals with
  | None ->
    let x' = fresh_var () in
    (x', { c with globals = Var.Map.add x x' c.globals })
  | Some x' -> (x', c)

let get_info x c =
  let (_, info) = find_ancestor_and_info x c in
  { info with initial = false }

let set_info x c info =
  let rec set_info x =
    match IMap.find x c.info with
    | Info _ -> IMap.add x (Info info) c.info
    | Son y -> set_info y
  in
  { c with info = set_info x }

let update_info x c f =
  get_info x c |> f |> set_info x c

(** {2 Info Helpers} *)

let get_feat f info = Feat.Map.find f info.feats
let set_feat f t info = { info with feats = Feat.Map.add f t info.feats }
let set_feat_if_none f t info = { info with feats = Feat.Map.update f (function Some t -> Some t | None -> Some t) info.feats }
let for_all_feats p info = Feat.Map.for_all p info.feats
let remove_feat f info = { info with feats = Feat.Map.remove f info.feats }
let remove_feats p info = { info with feats = Feat.Map.filter (fun f _ -> p f) info.feats }
let remove_all_feats info = { info with feats = Feat.Map.empty }

let has_fen info = info.fen

let set_fen info = { info with fen = true }

let remove_nfens info =
  { info with nfens = [] }

let not_implemented_nfens info =
  if info.nfens <> [] then
    not_implemented "nfens"

let not_implemented_nsims info =
  if info.nsims <> [] then
    not_implemented "nsims"

let remove_nsim x y c = (* FIXME: order of the arguments? *)
  update_info x c @@ fun info ->
  { info with nsims = List.filter (fun (_, z) -> z <> y) info.nsims }

let remove_nsims x c =
  let info = get_info x c in
  List.fold_left
    (fun c (_, y) -> remove_nsim y x c)
    (set_info x c { info with nsims = [] })
    info.nsims

let update_info_for_all_similarities upd x info c =
  List.fold_left
    (fun c (fs, z) ->
       update_info z c (upd fs z))
    (set_info x c (upd Feat.Set.empty x info))
    info.sims
