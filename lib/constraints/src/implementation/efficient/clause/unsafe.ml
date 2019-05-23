open Constraints_common
open Core

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

let nabs x f c =
  update_info x c @@ fun info ->
  match info.kind with
  | Dir dir ->
    (
      match Feat.Map.find_opt f dir.feats with
      | None | Some DontKnow ->
        Dnf.single
          { info with
            kind = Dir { dir with
                         feats = Feat.Map.add f Exists dir.feats } }
      | Some Exists | Some (Pointsto _) ->
        Dnf.single info
      | Some (Noresolve _) ->
        assert false (* FIXME *)
    )
  | _ ->
    Dnf.single
      { info with
        nabs = ExtList.insert_uniq_sorted Feat.compare f info.nabs }

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
      let ok_out =
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
  update_info x c @@ fun info ->
  match info.kind with
  | Dir dir ->
    Dnf.single
      { info with
        kind = Dir { dir with
                     sims = sim_update_sims_list fs y c.info dir.sims } }
  | _ -> assert false
