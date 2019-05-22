open Constraints_common
open Core

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

let fold_similar ?(if_=(fun _ -> true)) x f c =
  Dnf.bind c
    (fun c ->
       match (get_info x c).kind with
       | Dir dir ->
         List.fold_left
           (fun c (fs, y) ->
              Dnf.bind c
                (if if_ fs then f y else Dnf.single))
           (Dnf.single c)
           dir.sims
       | _ -> assert false)

let eq _x _y _c = assert false

let feat _x _f _y = assert false

let abs _x _f = assert false

let unsafe_nabs_in_nabs x f c =
  update_info x c @@ fun info ->
  Dnf.single
    { info with
      nabs = ExtList.insert_uniq_sorted Feat.compare f info.nabs }

let unsafe_nabs_in_dir x f c =
  update_info x c @@ fun info ->
  match info.kind with
  | Dir dir ->
    Dnf.single
      { info with
        kind = Dir { dir with
                     feats = Feat.Map.add f Exists dir.feats } }
  | _ -> assert false

let nabs x f c =
  match (get_info x c).kind with
  | Dir dir ->
    (
      match Feat.Map.find_opt f dir.feats with
      | None ->
        (
          fold_similar
            x
            ~if_:(fun fs -> not (Feat.Set.mem f fs))
            (fun y -> unsafe_nabs_in_dir y f)
            (unsafe_nabs_in_dir x f c)
        )
      | Some Exists | Some Pointsto _ -> Dnf.single c
      | Some (Noresolve _) -> Dnf.empty
    )
  | _ ->
    (
      fold_similar
        x
        ~if_:(fun fs -> not (Feat.Set.mem f fs))
        (fun y -> unsafe_nabs_in_nabs y f)
        (unsafe_nabs_in_nabs x f c)
    )

let fen _x _fs = assert false

let sim _x _fs _y = assert false

let set_empty_dir_and_suck_nabs info =
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

let kind k x c =
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
