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
  | Neg of Kind.t list
  | Pos of Kind.t

type feat =
  | DontKnow
  | Absent
  | Present of var
  | Maybe of var list

type info = {
  initial : bool ;
  kind : kind ;
  feats : feat Feat.Map.t ;
  fen : bool ;                         (* => dir *)
  sims : (Feat.Set.t * var) list ;  (* => dir *)
  nfeats : (Feat.t * var) list ;    (* => ¬ dir *)
  nabs : Feat.t list ;                 (* => ¬ dir *)
  nfens : Feat.Set.t list ;            (* => ¬ dir *)
  nsims : (Feat.Set.t * var) list ; (* => ¬ dir *)
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
  nfeats = [] ;
  nabs = [] ;
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

(** {2 Info Helpers} *)

let has_fen info = info.fen
