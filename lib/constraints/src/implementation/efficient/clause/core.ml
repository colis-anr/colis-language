open Constraints_common

(** {2 Variable} *)

type var = int

module IMap = Map.Make(struct type t = int let compare = compare end)

let fresh_var =
  let c = ref 0 in
  fun () -> incr c; !c

(** {2 Structure} *)

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

(** *)

let internalise x c =
  match Var.Map.find_opt x c.globals with
  | None ->
    let x' = fresh_var () in
    (x', { c with globals = Var.Map.add x x' c.globals })
  | Some x' -> (x', c)

let get_info x c =
  let rec get_info x =
    match IMap.find x c.info with
    | Info info -> info
    | Son y -> get_info y
  in
  { (get_info x) with initial = false }

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

let has_fen info = info.fen

let del_nfens info =
  { info with nfens = [] }

let del_nsim x y c = (* FIXME: order of the arguments? *)
  update_info x c @@ fun info ->
  { info with nsims = List.filter (fun (_, z) -> z <> y) info.nsims }

let del_nsims x c =
  let info = get_info x c in
  List.fold_left
    (fun c (_, y) -> del_nsim y x c)
    (set_info x c { info with nsims = [] })
    info.nsims
