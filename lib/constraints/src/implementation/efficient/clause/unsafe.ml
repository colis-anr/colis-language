open Constraints_common
open Core

let set_empty_dir_and_suck_nabs info = (* FIXME: remove and put in ensure_dir *)
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

let ensure_dir x c = (* FIXME: merge with dir *)
  match (get_info x c).kind with
  | Neg kinds when List.mem Kind.Dir kinds -> Dnf.empty
  | Neg _ | Any ->
    update_info x c @@ fun info -> Dnf.single (set_empty_dir_and_suck_nabs info)
  | Pos _ -> Dnf.empty
  | Dir _ -> Dnf.single c

let abs x f c =
  update_info x c @@ fun info ->
  match info.kind with
  | Dir dir ->
    (
      match Feat.Map.find_opt f dir.feats with
      | None | Some DontKnow | Some (Noresolve _) ->
        Dnf.single
          { info with
            kind = Dir { dir with
                         feats = Feat.Map.add f (Noresolve (C [])) dir.feats } }
      | Some Exists | Some (Pointsto _) -> Dnf.empty
    )
  | _ -> assert false

let nabs_in_nabs x f c =
  update_info x c @@ fun info ->
  Dnf.single
    { info with
      nabs = ExtList.insert_uniq_sorted Feat.compare f info.nabs }

let nabs_in_dir x f c =
  update_info x c @@ fun info ->
  match info.kind with
  | Dir dir ->
    Dnf.single
      { info with
        kind = Dir { dir with
                     feats = Feat.Map.add f Exists dir.feats } }
  | _ -> assert false

let fen x fs c =
  update_info x c @@ fun info ->
  match info.kind with
  | Dir dir ->
    (
      let (in_, out) =
        Feat.Map.partition
          (fun g _ -> Feat.Set.mem g fs)
          dir.feats
      in
      let ok_out = (* FIXME: No need to do it un the unsafe version. *)
        Feat.Map.for_all
          (fun _ -> function
             | Exists | Pointsto _ -> false
             | DontKnow | Noresolve _ -> true)
          out
      in
      if ok_out then
        (
          let in_ =
            Feat.Set.fold
              (fun f in_ ->
                 if Feat.Map.mem f in_ then
                   in_
                 else
                   Feat.Map.add f DontKnow in_)
              fs
              in_
          in
          Dnf.single
            { info with kind = Dir { dir with fen = true ; feats = in_ } }
        )
      else
        Dnf.empty
    )
  | _ -> assert false

let rec sim_update_sims_list fs y eqs = function
  | [] -> (* No previous sim between x and y. *)
    [fs, y]
  | (gs, z) :: l ->
    if IVar.equal eqs y z then (* Found the binding! *)
      (Feat.Set.inter fs gs, z) :: l
    else
      (gs, z) :: sim_update_sims_list fs y eqs l

let sim x fs y c =
  (update_info x c @@ fun info ->
   match info.kind with
   | Dir dir ->
     Dnf.single
       { info with
         kind = Dir { dir with
                      sims = sim_update_sims_list fs y c.info dir.sims } }
   | _ -> assert false
  )
