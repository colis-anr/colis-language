open Colis_constraints_common
open Dnf.Syntax

let do_nothing x = x (* fancy name for identity *)

let implied_by_ndir info c cont =
  (* This function is made to be used by constructs that have the {S-*-Kind} and
     {S-*-NDir} rules, that is the constructs that are known to be implied by
     a kind different from [dir] or by [¬dir]. It simply returns the clause when
     we know that this is not a directory. Or calls the continuation. *)
  match Core.Info.get_kind info with
  | Pos k when k <> Kind.Dir -> Dnf.single c (* S-*-Kind *)
  | Neg ks when List.mem Kind.Dir ks -> Dnf.single c (* S-*-NDir *)
  | _ -> cont ()

let implied_by_eq x y c cont =
  if Core.equal x y c then
    Dnf.single c
  else
    cont ()

let unsafe_sim x fs y c =
  Core.update_info x c @@ fun info ->
  Core.update_sim y
    (function
      | None -> fs
      | Some gs -> Feat.Set.inter fs gs)
    c info

let exists f c =
  (* fresh_var already returns an existentially quantified variable *)
  let (x, c) = Core.fresh_var c in
  f x c

let and_ r1 r2 = fun c ->
  c |> r1 |> List.map r2 |> List.flatten

let (&) = and_

let or_ r1 r2 = fun c ->
  (c |> r1) @ (c |> r2)

(** {2 Absence} *)

let abs x f c =
  let info = Core.get_info x c in
  implied_by_ndir info c @@ fun () -> (* S-Abs-Kind, S-Abs-NDir *)
  match Core.Info.get_feat f info with
  | None when Core.Info.has_fen info ->
    Core.(
      update_info_for_all_similarities
        (fun fs _ -> if Feat.Set.mem f fs
          then do_nothing
          else Info.remove_feat f (* P-Abs + S-Abs-Fen *))
        x info c
    ) |> Dnf.single

  | None | Some DontKnow | Some (Maybe _) | Some Absent -> (* S-Maybe-Abs *)
    Core.(
      update_info_for_all_similarities
        (fun fs _ -> if Feat.Set.mem f fs
          then do_nothing
          else Info.set_feat f Absent (* P-Abs *))
        x info c
    ) |> Dnf.single

  | Some (Present _) -> Dnf.empty (* C-Abs-Feat *)

(** {2 Kind} *)

let unsafe_kind_not_dir x info k c =
  Core.(
    info
    |> Info.remove_all_feats  (* S-Abs-NDir, S-Maybe-NDir *)
    |> Info.remove_nfens      (* S-NFen-NDir *)
    |> Info.set_kind k
    |> set_info x c
    |> remove_nsims x    (* S-NSim-NDir *)
    |> Dnf.single
  )

let kind x k c =
  let info = Core.get_info x c in

  match Core.Info.get_kind info with
  | Pos l when k = l ->
    Dnf.single c

  | Pos _ ->
    Dnf.empty (* C-Kind-Kind *)

  | Neg ls when List.mem k ls ->
    Dnf.empty (* C-Kind-NKind *)

  | _ -> (* (S-NKind-Kind) *)
    match k with
    | Dir ->
      Core.(
        info
        |> Info.set_kind (Pos Dir)
        |> set_info x c
        |> Dnf.single
      )

    | _ ->
      unsafe_kind_not_dir x info (Pos k) c

let dir x = kind x Kind.Dir

let missing_kind ls =
  (* Looks in a sorted list of kinds for the first one that isn't there by
     comparing to [Kind.all] (sorted as well). *)
  let rec missing_kind = function
    | l :: ls, k :: ks when Kind.equal l k ->
      missing_kind (ls, ks)
    | _, k :: _ ->
      k
    | _ ->
      assert false
  in
  missing_kind (ls, Kind.all)

let nkind x k c =
  let info = Core.get_info x c in

  match Core.Info.get_kind info with
  | Pos l when k = l -> Dnf.empty (* C-Kind-NKind *)
  | Pos _ -> Dnf.single c (* S-NKind-Kind *)
  | Neg ls when List.mem k ls -> Dnf.single c

  | Neg ls ->
    (
      let ls = ExtList.insert_uniq_sorted Kind.compare k ls in

      if List.length ls = Kind.nb_all - 1 then
        kind x (missing_kind ls) c

      else
        match k with
        | Kind.Dir ->
          unsafe_kind_not_dir x info (Neg ls) c

        | _ ->
          Core.(
            info
            |> Info.set_kind (Neg ls)
            |> set_info x c
            |> Dnf.single
          )
    )

  | Any ->
    (
      match k with
      | Kind.Dir ->
        unsafe_kind_not_dir x info (Neg [Dir]) c

      | _ ->
        Core.(
          info
          |> Info.set_kind (Neg [k])
          |> set_info x c
          |> Dnf.single
        )
    )

(** {2 Fence} *)

let fen x fs c =
  dir x c >>= fun c -> (* D-Fen *)
  let info = Core.get_info x c in
  (* Check that feats are either in the fence or outside but compatible. *)
  if
    Core.Info.for_all_feats
      (fun f t ->
         Feat.Set.mem f fs ||
         match t with
         | DontKnow | Absent | Maybe _ -> true
         | Present _ -> false (* C-Feat-Fen *))
      info
  then
    Core.(
      update_info_for_all_similarities (* P-Fen *)
        (fun gs _ info ->
           let hs = Feat.Set.union fs gs in
           Info.(
             info
             |> remove_feats (fun f -> not (Feat.Set.mem f hs)) (* S-Abs-Fen, S-Maybe-Fen *)
             |> Feat.Set.fold (fun f -> set_feat_if_none f DontKnow) hs
             |> set_fen
           ))
        x info c
    ) |> Dnf.single
  else
    Dnf.empty

(** {2 Feature} *)

let rec feat x f y c =
  dir x c >>= fun c ->
  let info = Core.get_info x c in
  match Core.Info.get_feat f info with
  | None when Core.Info.has_fen info -> Dnf.empty (* C-Feat-Fen *)
  | Some Absent -> Dnf.empty (* C-Abs-Feat *)
  | None | Some DontKnow ->
    Core.(
      update_info_for_all_similarities
        (fun fs _ -> if Feat.Set.mem f fs
          then do_nothing
          else Info.set_feat f (Present y) (* P-Feat *))
        x info c
    ) |> Dnf.single
  | Some (Present z) ->
    eq y z c
  | Some (Maybe zs) ->
    let c =
      Core.(
        update_info_for_all_similarities
          (fun fs _ -> if Feat.Set.mem f fs
            then do_nothing
            else Info.set_feat f (Present y) (* P-Feat *))
          x info c
      )
    in
    List.fold_left
      (fun c z -> eq y z =<< c) (* S-Maybe-Feat *)
      (Dnf.single c)
      zs

(** {2 Maybe} *)

and maybe x f y c =
  let info = Core.get_info x c in
  implied_by_ndir info c @@ fun () -> (* S-Maybe-Kind, S-Maybe-NDir *)
  match Core.Info.get_feat f info with
  | None when Core.Info.has_fen info -> Dnf.single c (* S-Maybe-Fen *)
  | Some Absent -> Dnf.single c (* S-Maybe-Abs *)
  | None | Some DontKnow ->
    Core.(
      update_info_for_all_similarities
        (fun fs _ -> if Feat.Set.mem f fs
          then do_nothing
          else Info.set_feat f (Maybe [y]) (* P-Maybe *))
        x info c
    ) |> Dnf.single
  | Some (Maybe zs) ->
    Core.(
      update_info_for_all_similarities
        (fun fs _ -> if Feat.Set.mem f fs
          then do_nothing
          else Info.set_feat f (Maybe (y :: zs)) (* P-Maybe *))
        x info c
    ) |> Dnf.single
  | Some (Present z) ->
    eq y z c (* S-Maybe-Feat *)

(** {2 Similarity} *)

and sim x fs y c =
  implied_by_eq x y c @@ fun () -> (* S-Sim-Relf *)
  dir x c >>= fun c -> (* D-Sim *)
  dir y c >>= fun c -> (* D-Sim *)
  let info_x = Core.get_info x c in
  let info_y = Core.get_info y c in
  let transfer_sims_manually x info_x fs y c =
    (* Transfer the sims. We don't do that by calling ourselves but
       manually. *)
    Core.(
      Info.fold_sims
        (fun gs z c ->
           let hs = Feat.Set.union fs gs in
           c >>= fun c ->
           c
           |> unsafe_sim y hs z
           |> unsafe_sim z hs y
           |> Dnf.single)
        (Dnf.single c)
        info_x
    ) >>= fun c ->

    (* Add our similarity. *)
    unsafe_sim x fs y c
    |> Dnf.single
  in
  transfer_info_but_sims info_x fs y c >>= fun c ->
  transfer_sims_manually x info_x fs y c >>= fun c ->
  transfer_info_but_sims info_y fs x c >>= fun c ->
  transfer_sims_manually y info_y fs x c

(** {2 Equality} *)

and eq x y c =
  implied_by_eq x y c @@ fun () -> (* S-Eq-Refl *)
  let info_x = Core.get_info x c in
  (
    match Core.Info.get_kind info_x with
   | Any -> Dnf.single c
   | Neg ks -> List.fold_left (fun c k -> nkind y k =<< c) (Dnf.single c) ks
   | Pos k -> kind y k c
  ) >>= fun c ->
  transfer_info_but_sims info_x Feat.Set.empty y c >>= fun c ->
  Core.(
    Info.fold_sims
      (fun gs z c -> sim y gs z =<< c)
      (Dnf.single c)
      info_x
  ) >>= fun c ->
  (* Remove all similarities that one has with [x], as we already moved them *)
  let c =
    Core.(
      update_info_for_all_similarities
        (fun _ _ ->
           remove_sim x c)
        x info_x c
    )
  in
  Core.identify x y (fun _ info_y -> info_y) c
  |> Dnf.single

(** {2 Info Transfer} *)

and transfer_info_but_sims info_x fs y c =
  (* This function defines the transfer of info from [x] to [y].

     The [fs] set of features will be added to the sets of features in the
     fence when transfering it.

     It does not handle transfering similarities because that treatment requires
     a very specific treatment.

     Also, it only handles transfer from [x] to [y], and not the other
     direction: it will need to be called both ways. *)

  (* Fail in case of nfens or nsims. FIXME. *)
  Core.Info.not_implemented_nfens info_x;
  Core.Info.not_implemented_nsims info_x;

  (* Transfer all feats by calling the appropriate function
     (feat, abs or maybe). *)
  Core.(
    Info.fold_feats
      (fun f t c ->
         if Feat.Set.mem f fs then
           c
         else
         c >>= fun c ->
         match t with
         | DontKnow -> Dnf.single c
         | Absent -> abs y f c
         | Present z -> feat y f z c
         | Maybe zs -> List.fold_left (fun c z -> maybe y f z =<< c) (Dnf.single c) zs
      )
      (Dnf.single c)
      info_x
  ) >>= fun c ->

  ( (* Transfer the fen if there is one. *)
    if Core.Info.has_fen info_x then
      let gs = Core.Info.fold_feats (fun f _ fs -> Feat.Set.add f fs) Feat.Set.empty info_x in
      let hs = Feat.Set.union fs gs in
      fen y hs c
    else
      Dnf.single c
  )

(** {2 ¬ Absence} *)

let nabs x f = exists @@ fun y -> feat x f y

(** {2 Non-Equality} *)

let neq _x _y =
  Core.not_implemented "neq"

let nfeat x f y =
  exists @@ fun z ->
  maybe x f z & neq y z

let nmaybe x f y =
  exists @@ fun z ->
  feat x f y & neq y z

(** {2 Macros} *)

let rec resolve x pi q z =
  match Path.split_first_rel q with
  | None -> eq x z
  | Some (Down f, q) ->
    exists @@ fun y ->
    feat x f y & resolve y (x :: pi) q z
  | Some (Here, q) ->
    resolve x pi q z
  | Some (Up, q) ->
    match pi with
    | [] -> resolve x [] q z
    | y::pi -> dir x & resolve y pi q z

let rec noresolve x pi q =
  (* FIXME: redefine using `maybe_resolve`? *)
  match Path.split_first_rel q with
  | None -> (fun _ -> []) (* false *)
  | Some (Down f, q) ->
    (match Path.split_first_rel q with
     | None ->
       abs x f
     | _ ->
       exists @@ fun y ->
       maybe x f y & noresolve y (x::pi) q)
  | Some (Here, q) ->
    noresolve x pi q
  | Some (Up, q) ->
    match pi with
    | [] -> noresolve x [] q
    | y::pi -> or_ (nkind x Kind.Dir) (noresolve y pi q)

let rec maybe_resolve x pi q z =
  match Path.split_first_rel q with
  | None -> eq x z
  | Some (Down f, q) ->
    exists @@ fun y ->
    maybe x f y & maybe_resolve y (x::pi) q z
  | Some (Here, q) ->
    maybe_resolve x pi q z
  | Some (Up, q) ->
    match pi with
    | [] -> maybe_resolve x [] q z
    | y::pi -> or_ (nkind x Kind.Dir) (maybe_resolve y pi q z)

let rec similar x x' p z z' =
  match p with
  | [] ->
    eq x z & eq x' z'
  | f::p ->
    exists @@ fun y -> exists @@ fun y' ->
    feat x f y & feat x' f y' & sim x (Feat.Set.singleton f) x' & similar y y' p z z'
