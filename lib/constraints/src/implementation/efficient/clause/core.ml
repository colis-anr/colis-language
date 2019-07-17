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

let pp_debug fmt c =
  let fpf = Format.fprintf in
  let pp_globals fmt =
    Var.Map.iter (fpf fmt "%a -> %d@\n" Var.pp)
  in
  let pp_eqs fmt =
    IMap.iter (fun x -> function
        | Son y -> fpf fmt "%d -> %d@\n" x y
        | Info _ -> ())
  in
  let pp_info fmt info =
    fpf fmt "initial: %b@\n" info.initial;
    fpf fmt "kind: ";
    (match info.kind with
     | Any -> fpf fmt "*"
     | Neg ks -> fpf fmt "¬"; Format.pp_print_list ~pp_sep:(fun fmt () -> fpf fmt ", ¬") Kind.pp fmt ks
     | Pos k -> Kind.pp fmt k);
    fpf fmt "@\n";
    fpf fmt "feats:@\n";
    Feat.Map.iter
      (fun f t ->
         fpf fmt "  %a -> " Feat.pp f;
         (match t with
          | DontKnow -> fpf fmt "?"
          | Absent -> fpf fmt "X"
          | Present y -> fpf fmt "%d" y
          | Maybe ys -> fpf fmt "maybe: "; Format.pp_print_list ~pp_sep:(fun fmt () -> fpf fmt ", ") Format.pp_print_int fmt ys);
         fpf fmt "@\n")
      info.feats;
    fpf fmt "fen: %b@\n" info.fen;
    fpf fmt "sims:@\n";
    List.iter
      (fun (fs, z) ->
         fpf fmt "  ~%a %d@\n" Feat.Set.pp fs z)
      info.sims;
    fpf fmt "nfens:@\n";
    List.iter
      (fun fs ->
         fpf fmt "  ¬%a@\n" Feat.Set.pp fs)
      info.nfens;
    fpf fmt "nsims:@\n";
    List.iter
      (fun (fs, z) ->
         fpf fmt "  ¬~%a %d@\n" Feat.Set.pp fs z)
      info.nsims
  in
  let pp_infos fmt =
    IMap.iter (fun x -> function
        | Son _ -> ()
        | Info info -> fpf fmt "@[<h 2>%d:@\n%a@]@\n" x pp_info info)
  in
  fpf fmt "@[<h 2>globals:@\n%a@]@\n@[<h 2>equalities:@\n%a@]@\n@[<h 2>info:@\n%a@]"
    pp_globals c.globals pp_eqs c.info pp_infos c.info

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
  let x = ref 0 in
  fun c ->
    incr x;
    (!x, { c with info = IMap.add !x (Info empty_info) c.info })

let hash x = x

(** {2 Variables} *)

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

let identify x y merge c =
  let rec identify x y =
    match IMap.find x c.info, IMap.find y c.info with
    | Info info_x, Info info_y ->
      let info =
        c.info
        |> IMap.add x (Son y)
        |> IMap.add y (Info (merge info_x info_y))
      in
      { c with info }
    | Info _, Son y -> identify x y
    | Son x, Info _ -> identify x y
    | Son x, Son y -> identify x y
  in
  identify x y

let internalise gx c =
  match Var.Map.find_opt gx c.globals with
  | None ->
    let (x, c) = fresh_var c in
    (x, { c with globals = Var.Map.add gx x c.globals })
  | Some x -> (x, c)

let externalise x c =
  Var.Map.fold
    (fun gy y gxs ->
       if x = y then
         gy :: gxs
       else
         gxs)
    c.globals
    []

let quantify_over x c =
  { c with globals = Var.Map.remove x c.globals }

let make_initial c =
  { c with
    info =
      IMap.map
        (function
          | Son y -> Son y
          | Info info -> Info { info with initial = true })
        c.info }

let get_info x c =
  let (_, info) = find_ancestor_and_info x c in
  { info with initial = false }

let is_initial info = info.initial

let set_info x c info =
  let rec set_info x =
    match IMap.find x c.info with
    | Info _ -> IMap.add x (Info info) c.info
    | Son y -> set_info y
  in
  { c with info = set_info x }

let update_info x c f =
  get_info x c |> f |> set_info x c

let iter f c =
  IMap.iter
    (fun x -> function
       | Son _ -> ()
       | Info info -> f x info)
    c.info

let iter_equalities f c =
  IMap.iter
    (fun x -> function
       | Son y -> f x y
       | Info _ -> ())
    c.info

(** {2 Info Helpers} *)

(** {3 Kinds} *)

let get_kind info = info.kind

let set_kind kind info = { info with kind }

(** {3 Features} *)

let get_feat f info = Feat.Map.find_opt f info.feats

let iter_feats f info = Feat.Map.iter f info.feats

let fold_feats f e info = Feat.Map.fold f info.feats e

let for_all_feats p info = Feat.Map.for_all p info.feats

let set_feat f t info = { info with feats = Feat.Map.add f t info.feats }

let set_feat_if_none f t info = { info with feats = Feat.Map.update f (function Some t -> Some t | None -> Some t) info.feats }

let remove_feat f info = { info with feats = Feat.Map.remove f info.feats }

let remove_feats p info = { info with feats = Feat.Map.filter (fun f _ -> p f) info.feats }

let remove_all_feats info = { info with feats = Feat.Map.empty }

(** {3 Fences} *)

let has_fen info = info.fen

let set_fen info = { info with fen = true }

(** {3 Similarities} *)

let update_sim y upd c info =
  let rec update_sim = function
    | [] -> [upd None, y]
    | (fs, z) :: sims when equal y z c -> (upd (Some fs), z) :: sims
    | (fs, z) :: sims -> (fs, z) :: update_sim sims
  in
  { info with sims = update_sim info.sims }

let iter_sims f info =
  List.iter (fun (fs, z) -> f fs z) info.sims

let fold_sims f e info =
  List.fold_left
    (fun e (fs, z) -> f fs z e)
    e info.sims

let update_info_for_all_similarities upd x info c =
  List.fold_left
    (fun c (fs, z) ->
       update_info z c (upd fs z))
    (set_info x c (upd Feat.Set.empty x info))
    info.sims

(** {3 ¬ Fences} *)

let remove_nfens info =
  { info with nfens = [] }

let not_implemented_nfens info =
  if info.nfens <> [] then
    not_implemented "nfens"

(** {3 ¬ Similarities} *)

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
