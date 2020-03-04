open Colis_constraints_common

exception NotImplemented of string
let not_implemented s = raise (NotImplemented s)

type var = int
[@@deriving yojson]

module VarSet = Set.Make(struct type t = int let compare = compare end)

let shadow = ref false

let with_shadow_variables f =
  let shadow_bak = !shadow in
  shadow := true;
  let b = try Ok (f ()) with e -> Error e in
  shadow := shadow_bak;
  match b with Ok v -> v | Error e -> raise e

module Info = struct
  type kind =
    | Any
    | Neg of Kind.t list (* uniques, sorted, less than (#kinds - 1) elements *)
    | Pos of Kind.t
  [@@deriving yojson]

  type feat =
    | DontKnow
    | Absent
    | Present of var    (* implies dir *)
    | Maybe of var list
  [@@deriving yojson]

  type t = {
    shadow : bool ;
    kind : kind ;
    feats : feat Feat.Map.t ;
    fen : bool ;                      (* implies dir *)
    sims : (Feat.Set.t * var) list ;  (* max 1 for each variable, implies dir *)
    nfens : Feat.Set.t list ;         (* only if not "not dir" *)
    nsims : (Feat.Set.t * var) list ; (* only if not "not dir" *)
  }
  [@@deriving yojson]

  (* FIXME: Could maybe be a ref instead of a function. Faster? *)
  let empty_info () = {
    shadow  = !shadow;
    kind = Any ;
    feats = Feat.Map.empty ;
    fen = false ;
    sims = [] ;
    nfens = [] ;
    nsims = [] ;
  }

  let is_shadow info = info.shadow

  let update_shadow info =
    { info with shadow = info.shadow && !shadow }

  let get_kind info = info.kind

  let set_kind kind info = { info with kind }

  let get_feat f info = Feat.Map.find_opt f info.feats

  let iter_feats f info = Feat.Map.iter f info.feats

  let fold_feats f e info = Feat.Map.fold f info.feats e

  let for_all_feats p info = Feat.Map.for_all p info.feats

  let set_feat f t info = { info with feats = Feat.Map.add f t info.feats }

  let set_feat_if_none f t info = { info with feats = Feat.Map.update f (function Some t -> Some t | None -> Some t) info.feats }

  let remove_feat f info = { info with feats = Feat.Map.remove f info.feats }

  let remove_feats p info = { info with feats = Feat.Map.filter (fun f _ -> p f) info.feats }

  let remove_all_feats info = { info with feats = Feat.Map.empty }

  let has_fen info = info.fen

  let set_fen info = { info with fen = true }

  let iter_sims f info =
    List.iter (fun (fs, z) -> f fs z) info.sims

  let fold_sims f e info =
    List.fold_left
      (fun e (fs, z) -> f fs z e)
      e info.sims

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

end
























module IMap = Derivable.MakeMap(Derivable.Int)

type info_or_son = Info of Info.t | Son of var
[@@deriving yojson]

type t = {
  (* Globals: not existentially quantified.
     Map from external variables to internal. *)
  globals : var Var.Map.t ;

  (* Info: a union-find carrying [info]. *)
  info : info_or_son IMap.t ;
}
[@@deriving yojson]

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
    fpf fmt "shadow: %b@\n" info.Info.shadow;
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
          | Info.DontKnow -> fpf fmt "?"
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

let fresh_var =
  let x = ref 0 in
  fun c ->
    incr x;
    (!x, { c with info = IMap.add !x (Info (Info.empty_info ())) c.info })

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

let syntactic_compare = compare

let identify x y merge c =
  let rec identify x y =
    match IMap.find x c.info, IMap.find y c.info with
    | Info info_x, Info info_y ->
      let info_x = Info.update_shadow info_x in
      let info_y = Info.update_shadow info_y in
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

let fold_globals f c =
  Var.Map.fold (fun _ -> f) c.globals

let quantify_over x c =
  { c with globals = Var.Map.remove x c.globals }

let get_info_no_shadow x c = find_ancestor_and_info x c |> snd
let get_info x c = get_info_no_shadow x c |> Info.update_shadow

let is_shadow x c =
  (* We can't use [get_info] because it can set the shadow field to false. *)
  let (_, info) = find_ancestor_and_info x c in
  Info.is_shadow info

let set_info x c info =
  let rec set_info x =
    match IMap.find x c.info with
    | Info _ -> IMap.add x (Info info) c.info
    | Son y -> set_info y
  in
  { c with info = set_info x }

let update_info x c f =
  get_info x c |> f |> set_info x c

let fold_infos f c e =
  IMap.fold
    (fun x i e ->
       match i with
       | Son _ -> e
       | Info info -> f x info e)
    c.info
    e

let iter_infos f c =
  IMap.iter
    (fun x -> function
       | Son _ -> ()
       | Info info -> f x info)
    c.info

let filter_infos p c =
  { c with info = IMap.filter (fun x _ -> p x) c.info }

let iter_equalities f c =
  IMap.iter
    (fun x -> function
       | Son y -> f x y
       | Info _ -> ())
    c.info


(** {3 Similarities} *)

let remove_sim y c info =
  Info.{ info with
         sims =
           List.filter
             (fun (_, z) ->
                not (equal y z c))
             info.sims }

let update_sim y upd c info =
  let rec update_sim = function
    | [] -> [upd None, y]
    | (fs, z) :: sims when equal y z c -> (upd (Some fs), z) :: sims
    | (fs, z) :: sims -> (fs, z) :: update_sim sims
  in
  Info.{ info with sims = update_sim info.sims }

let update_info_for_all_similarities upd x info c =
  List.fold_left
    (fun c (fs, z) ->
       update_info z c (upd fs z))
    (set_info x c (upd Feat.Set.empty x info))
    info.sims

let remove_nsim x y c = (* FIXME: order of the arguments? *)
  update_info x c @@ fun info ->
  { info with nsims = List.filter (fun (_, z) -> z <> y) info.nsims }

let remove_nsims x c =
  let info = get_info x c in
  List.fold_left
    (fun c (_, y) -> remove_nsim y x c)
    (set_info x c { info with nsims = [] })
    info.nsims

(** {2 Garbage Collection} *)

let rec list_map_filter f = function
  | [] -> []
  | h :: q ->
    match f h with
    | None -> list_map_filter f q
    | Some h -> h :: list_map_filter f q

module VSet = Set.Make(struct type t = var let compare = syntactic_compare end)

let find_ancestor x c =
  fst (find_ancestor_and_info x c)

let simplify c =
  (* The simplification uses the theorem that tells that we can remove all the
     literals about inaccessible (wrt the feature constraint) variables safely,
     this is an equivalence. The [simplify] function is thus some sort of
     garbage collector. It makes the constraint nicer to look at as a human
     being. Also, by making the constraint smaller, it can improve the speed of
     subsequent modifications.
     This operation is pretty costly: #literals * log(#variables). *)
  let rec gather_accessibles_from accessibles x =
    Info.fold_feats
      (fun _ t accessibles ->
         match t with
         | DontKnow | Absent -> accessibles
         | Present y -> gather_accessibles_from accessibles y
         | Maybe ys -> List.fold_left gather_accessibles_from accessibles ys)
      (VSet.add x accessibles)
      (get_info x c)
  in
  let accessibles =
    fold_globals
      (fun r accessibles ->
         gather_accessibles_from accessibles r)
      c
      VSet.empty
  in
  let accessibles = VSet.map (fun x -> find_ancestor x c) accessibles in
  let is_accessible x =
    VSet.mem x accessibles
  in
  (* To be sure not to remove equalities, we get rid of them first. To do that,
     we lift everything to the representents in the union-find. *)
  let globals = Var.Map.map (fun x -> find_ancestor x c) c.globals in
  let info =
    c.info
    |> IMap.filter
      (fun x -> function
         (* ...and we filter out all the sons *)
         | Son _ -> false
         (* and keep only the ancestors that satisfy [p]. *)
         | Info _ -> is_accessible x)
    |> IMap.map
      (* We then take care of the infos. To do that, we lift everything to the
         ancestors and then remove everything that is about an ancestor that
         does not satisfy [p]. *)
      (function
        | Son _ -> assert false
        | Info info ->
          Info
            { shadow = info.shadow ;
              kind = info.kind ;
              feats =
                Feat.Map.map_filter
                  (function
                    | Info.DontKnow -> Some Info.DontKnow
                    | Absent -> Some Absent
                    | Present y ->
                      let y =  find_ancestor y c in
                      if is_accessible y then Some (Present y) else None
                    | Maybe ys ->
                      let ys =
                        ys
                        |> List.map (fun y -> find_ancestor y c)
                        |> List.filter is_accessible
                        |> List.sort_uniq syntactic_compare
                      in
                      if ys = [] then None else Some (Maybe ys))
                  info.feats ;
              fen = info.fen ;
              sims =
                list_map_filter
                  (fun (fs, y) ->
                     let y = find_ancestor y c in
                     if is_accessible y then Some (fs, y) else None)
                  info.sims ;
              nfens = info.nfens ;
              nsims =
                list_map_filter
                  (fun (fs, y) ->
                     let y = find_ancestor y c in
                     if is_accessible y then Some (fs, y) else None)
                  info.nsims })
  in
  { globals ; info }
