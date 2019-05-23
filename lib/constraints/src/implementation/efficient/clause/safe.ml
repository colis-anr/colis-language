open Constraints_common
open Core
open Dnf.Syntax

let fold_similar ?(if_=(fun _ _ -> true)) x f c =
  c >>= fun c ->
  match (get_info x c).kind with
  | Dir dir ->
    List.fold_left
      (fun c (fs, y) ->
         c >>= (if if_ fs y then f fs y else Dnf.single))
      (Dnf.single c)
      dir.sims
  | _ -> assert false

exception NotImplemented of string
let not_implemented s = raise (NotImplemented s)

let eq _x _y _c = assert false

let feat _x _f _y = assert false

let abs x f c = (* FIXME: subsumed by noresolve *)
  Unsafe.ensure_dir x c >>= fun c ->
  fold_similar
    x
    ~if_:(fun fs _ -> not (Feat.Set.mem f fs))
    (fun _ y -> Unsafe.abs y f)
    (Unsafe.abs x f c)

let nabs x f c =
  match (get_info x c).kind with
  | Dir dir ->
    (
      match Feat.Map.find_opt f dir.feats with
      | None | Some DontKnow ->
        (
          fold_similar
            x
            ~if_:(fun fs _ -> not (Feat.Set.mem f fs))
            (fun _ y -> Unsafe.nabs_in_dir y f)
            (Unsafe.nabs_in_dir x f c)
        )
      | Some Exists | Some Pointsto _ -> Dnf.single c
      | Some (Noresolve _) -> Dnf.empty (* FIXME: wrong. Noresolve doesnt clash with nabs, we have to unfold. *)
    )
  | _ ->
    (
      fold_similar
        x
        ~if_:(fun fs _ -> not (Feat.Set.mem f fs))
        (fun _ y -> Unsafe.nabs_in_nabs y f)
        (Unsafe.nabs_in_nabs x f c)
    )

let fen x fs c =
  Unsafe.ensure_dir x c >>= fun c ->
  fold_similar
    x
    (fun gs y -> Unsafe.fen y (Feat.Set.union fs gs))
    (Unsafe.fen x fs c)

let sim x fs y c =
  (* To have a similarity implies being directories. *)
  Unsafe.ensure_dir x c >>= fun c ->
  Unsafe.ensure_dir y c >>= fun c ->
  let info_x = get_info x c in
  let info_y = get_info y c in
  match info_x.kind, info_y.kind with
  | Dir dir_x, Dir dir_y ->
    (
      (* There are now two cases. *)
      match List.find_opt (fun (_, xi) -> IVar.equal c.info y xi) dir_x.sims with
      | None -> (* No previous similarity between [x] and [y]. *)
        (
          assert false (* FIXME *)
        )
      | Some _fs' -> (* The general case, which will not be implemented for now. *)
        not_implemented "sim: general case"
    )
  | _ -> assert false

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
      | Any -> Dnf.single (Unsafe.set_empty_dir_and_suck_nabs info)
      | Neg kinds when List.mem Kind.Dir kinds -> Dnf.empty
      | Neg _ -> Dnf.single (Unsafe.set_empty_dir_and_suck_nabs info)
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
          | Kind.Dir -> Dnf.single (Unsafe.set_empty_dir_and_suck_nabs info)
          | kind -> Dnf.single (set_pos_and_empty_negs info kind)
        else
          Dnf.single {info with kind = Neg kinds}
      | Pos kind when Kind.equal kind k -> Dnf.empty
      | Pos _ -> Dnf.single info
      | Dir _ -> Dnf.single info
    )
