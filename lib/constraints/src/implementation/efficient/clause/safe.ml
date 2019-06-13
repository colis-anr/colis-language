open Constraints_common
open Core
open Dnf.Syntax

let fold_similar ?(if_=(fun _ _ -> true)) x f c =
  match (get_info x c).kind with
  | Dir dir ->
    List.fold_left
      (fun c (fs, y) ->
         c >>= (if if_ fs y then f fs y else Dnf.single))
      (Dnf.single c)
      dir.sims
  | _ ->
    Dnf.single c

exception NotImplemented of string
let not_implemented s = raise (NotImplemented s)

let set_empty_dir_and_suck_nabs info = (* FIXME: remove and put in dir *)
  let feats =
    List.fold_left
      (fun feats f ->
         Feat.Map.add f Exists feats)
      Feat.Map.empty
      info.nabs
  in
  { info with
    nfeats = [] ;
    kind = Dir { fen = false ; sims = [] ; feats } }

let dir x c = (* FIXME: No need to handle that case in [kind], then. *)
  update_info x c @@ fun info ->
  match info.kind with
  | Neg kinds when List.mem Kind.Dir kinds ->
    Dnf.empty
  | Neg _ | Any ->
    Dnf.single (set_empty_dir_and_suck_nabs info)
  | Pos _ ->
    Dnf.empty
  | Dir _ ->
    Dnf.single info


let abs x f c = (* FIXME: subsumed by noresolve *)
  dir x c >>= fun c ->
  Unsafe.abs x f c >>= fun c ->
  fold_similar
    x
    ~if_:(fun fs _ -> not (Feat.Set.mem f fs))
    (fun _ y -> Unsafe.abs y f)
    c

let nabs x f c =
  Unsafe.nabs x f c >>= fun c ->
  fold_similar
    x
    ~if_:(fun fs _ -> not (Feat.Set.mem f fs))
    (fun _ y -> Unsafe.nabs y f)
    c

let fen x fs c =
  dir x c >>= fun c ->
  Unsafe.fen x fs c >>= fun c ->
  fold_similar
    x
    (fun gs y -> Unsafe.fen y (Feat.Set.union fs gs))
    c

let set_pos_and_empty_negs _info k =
  { nfeats = [] ;
    nabs = [] ;
    nfens = [] ;
    nsims = [] ; (* FIXME: we need to remove the refl versions. *)
    kind = Pos k }

let set_ndir_and_empty_negs _info =
  { nfeats = [] ;
    nabs = [] ;
    nfens = [] ;
    nsims = [] ; (* FIXME: we need to remove the refl versions. *)
    kind = Neg [Dir] }

let kind k x c = (* FIXME: separate dir from kind *)
  update_info x c @@ fun info ->
  match k with
  | Kind.Dir ->
    (
      match info.kind with
      | Any -> Dnf.single (set_empty_dir_and_suck_nabs info)
      | Neg kinds when List.mem Kind.Dir kinds -> Dnf.empty
      | Neg _ -> Dnf.single (set_empty_dir_and_suck_nabs info)
      | Pos _ -> Dnf.empty
      | Dir _ -> Dnf.single info
    )
  | _ ->
    (
      match info.kind with
      | Any -> Dnf.single (set_pos_and_empty_negs info k)
      | Neg kinds when List.mem k kinds -> Dnf.empty
      | Neg _ -> Dnf.single (set_pos_and_empty_negs info k)
      | Pos kind when Kind.equal k kind -> Dnf.single info
      | Pos _ -> Dnf.empty
      | Dir _ -> Dnf.empty
    )

let rec find_smallest_diff cmp l1 l2 =
  match l1, l2 with
  | [], [] -> failwith "find_smallest_diff"
  | [], h2 :: _ -> h2
  | h1 :: _, [] -> h1
  | h1 :: q1, h2 :: q2 ->
    match cmp h1 h2 with
    | c when c < 0 -> h1
    | c when c > 0 -> h2
    | _ -> find_smallest_diff cmp q1 q2

let nkind k x c =
  update_info x c @@ fun info ->
  match k with
  | Kind.Dir ->
    (
      match info.kind with
      | Any -> Dnf.single (set_ndir_and_empty_negs info)
      | Neg kinds ->
        let kinds = ExtList.insert_uniq_sorted Kind.compare Kind.Dir kinds in
        if List.length kinds = Kind.nb_all - 1 then
          Dnf.single (set_pos_and_empty_negs info (find_smallest_diff Kind.compare kinds Kind.all))
        else
          Dnf.single {info with kind = Neg kinds}
      | Pos _ -> Dnf.single info
      | Dir _ -> Dnf.empty
    )
  | _ ->
    (
      match info.kind with
      | Any ->  assert (Kind.nb_all > 2); Dnf.single {info with kind = Neg [k]}
      | Neg kinds ->
        let kinds = ExtList.insert_uniq_sorted Kind.compare k kinds in
        if List.length kinds = Kind.nb_all - 1 then
          match find_smallest_diff Kind.compare kinds Kind.all with
          | Kind.Dir -> Dnf.single (set_empty_dir_and_suck_nabs info)
          | kind -> Dnf.single (set_pos_and_empty_negs info kind)
        else
          Dnf.single {info with kind = Neg kinds}
      | Pos kind when Kind.equal kind k -> Dnf.empty
      | Pos _ -> Dnf.single info
      | Dir _ -> Dnf.single info
    )

let rec feat x f y c =
  dir x c >>= fun c ->
  let info = get_info x c in
  match info.kind with
  | Dir d ->
    (
      match Feat.Map.find_opt f d.feats with
      | None when d.fen ->
        Dnf.empty
      | None | Some DontKnow | Some Exists ->
        Unsafe.feat x f y c >>= fun c ->
        fold_similar
          x
          ~if_:(fun fs _ -> not (Feat.Set.mem f fs))
          (fun _ z -> Unsafe.feat z f y)
          c
      | Some (Pointsto z) ->
        eq y z c
      | Some (Noresolve (C [])) -> (* FIXME: subsumed by the next case *)
        Dnf.empty
      | Some (Noresolve _) ->
        not_implemented "feat: noresolve"
    )
  | _ -> assert false

and sim_suck_info fs ~from ~to_ cs =
  let (info_x, d_x) = from in
  let y = to_ in

  if info_x.nfeats <> [] then
    not_implemented "sim: nfeats";

  assert (info_x.nabs = []); (* Because directory! *)

  if info_x.nfens <> [] then
    not_implemented "sim: nfens";

  if info_x.nsims <> [] then
    not_implemented "sim: nsims";

  let cs =
    if d_x.fen then
      (
        (* FIXME: could be done much more efficiently, by exposing the
           right low-level function common to eq and fen. *)
        let gs =
          Feat.Map.fold
            (fun g _ gs -> Feat.Set.add g gs)
            d_x.feats
            Feat.Set.empty
        in
        fen y (Feat.Set.union fs gs) =<< cs
      )
    else
      cs
  in

  (* Do not suck similarities, this will be handled in [sim]. *)

  let cs =
    Feat.Map.fold
      (fun f t cs ->
         if Feat.Set.mem f fs then
           cs
         else
           match t with
           | DontKnow -> cs
           | Exists -> nabs y f =<< cs
           | Pointsto z -> feat y f z =<< cs
           | Noresolve (C []) -> abs y f =<< cs (* FIXME: subsumed *)
           | Noresolve _ -> not_implemented "sim: noresolve")
      d_x.feats
      cs
  in

  cs

and sim x fs y c =
  (* To have a similarity implies being directories. *)
  dir x c >>= fun c ->
  dir y c >>= fun c ->
  let info_x = get_info x c in
  let info_y = get_info y c in
  match info_x.kind, info_y.kind with
  | Dir d_x, Dir d_y ->
    (
      (* There are now two cases. *)
      match List.find_opt (fun (_, xi) -> IVar.equal c.info y xi) d_x.sims with
      | None -> (* No previous similarity between [x] and [y]. *)
        (
          let cs = Dnf.single c in
          let cs = sim_suck_info fs ~from:(info_x, d_x) ~to_:y cs in
          let cs = sim_suck_info fs ~from:(info_y, d_y) ~to_:x cs in

          cs >>=
          Unsafe.sim x fs y >>= fun c ->
          List.fold_left
            (fun cs (gs, xi) ->
               let fsgs = Feat.Set.union fs gs in
               cs >>=
               Unsafe.sim xi fsgs y >>= fun c ->
               List.fold_left
                 (fun cs (hs, yj) ->
                    cs >>=
                    Unsafe.sim xi (Feat.Set.union fsgs hs) yj)
                 (Dnf.single c)
                 d_y.sims
            )
            (Dnf.single c)
            d_x.sims
        )
      | Some _fs' -> (* The general case, which will not be implemented for now. *)
        not_implemented "sim: general case"
    )
  | _ -> assert false

(* We basically suck all the information from one of the two variables. At
   each step, we are in normal form. At the end, we can just remove all
   information from this variable and state the equality. *)
(* FIXME: find a way to suck info out of the smallest one. For now, we'll
   suck them from [y]. *)
and eq x y c =
  if IVar.equal c.info x y then
    Dnf.single c
  else
    (
      let info_y = get_info y c in

      let cs = Dnf.single c in

      if info_y.nfeats <> [] then
        not_implemented "eq: nfeats"; (* FIXME eventually *)

      let cs = List.fold_left (fun cs f -> nabs x f =<< cs) cs info_y.nabs in

      if info_y.nfens <> [] then
        not_implemented "eq: nfens"; (* FIXME eventually *)

      if info_y.nsims <> [] then
        not_implemented "eq: nsims"; (* FIXME eventually *)

      let cs =
        match info_y.kind with
        | Any -> cs
        | Neg kinds -> List.fold_left (fun cs k -> nkind k x =<< cs) cs kinds
        | Pos k -> kind k x =<< cs
        | Dir d ->
          let cs = dir x =<< cs in

          let cs =
            if d.fen then
              (
                (* FIXME: could be done much more efficiently, by exposing the
                   right low-level function common to eq and fen. *)
                let gs =
                  Feat.Map.fold
                    (fun g _ gs -> Feat.Set.add g gs)
                    d.feats
                    Feat.Set.empty
                in
                fen x gs =<< cs
              )
            else
              cs
          in

          let cs = List.fold_left (fun cs (hs, z) -> sim x hs z =<< cs) cs d.sims in

          let cs =
            Feat.Map.fold
              (fun f t cs ->
                 match t with
                 | DontKnow -> cs
                 | Exists -> nabs x f =<< cs
                 | Pointsto z -> feat x f z =<< cs
                 | Noresolve (C []) -> abs x f =<< cs (* FIXME: once the general case is handled, no need! *)
                 | Noresolve _ -> assert false)
              d.feats
              cs
          in

          cs
      in

      List.map
        (fun c ->
           { c with
             info = IVar.set_equal c.info x y (fun info_x _ -> info_x) })
        cs
    )