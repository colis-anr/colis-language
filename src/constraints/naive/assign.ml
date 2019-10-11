open Colis_constraints_common

type t =
  { vars : Var.t Metavar.Map.t ;
    feats : Feat.t Metavar.Map.t ;
    kinds : Kind.t Metavar.Map.t ;
    feat_sets : Feat.Set.t Metavar.Map.t }

let var aff m = Metavar.Map.find m aff.vars
let feat aff m = Metavar.Map.find m aff.feats
let kind aff m = Metavar.Map.find m aff.kinds
let feat_set aff m = Metavar.Map.find m aff.feat_sets

let empty =
  { vars = Metavar.Map.empty ;
    feats = Metavar.Map.empty ;
    kinds = Metavar.Map.empty ;
    feat_sets = Metavar.Map.empty }

let from_lists ?(vars=[]) ?(feats=[]) ?(kinds=[]) ?(feat_sets=[]) () =
  let rec map_add_list equal l m =
    match l with
    | [] -> Some m
    | (hk, hv) :: t ->
       match Metavar.Map.find_opt hk m with
       | None -> map_add_list equal t (Metavar.Map.add hk hv m)
       | Some hv' ->
          if equal hv hv' then
            map_add_list equal t m
          else
            None
  in
  let (>>=) = BatOption.bind in
  map_add_list Var.equal vars Metavar.Map.empty >>= fun vars ->
  map_add_list Feat.equal feats Metavar.Map.empty >>= fun feats ->
  map_add_list Kind.equal kinds Metavar.Map.empty >>= fun kinds ->
  map_add_list Feat.Set.equal feat_sets Metavar.Map.empty >>= fun feat_sets ->
  Some { vars ; feats ; kinds ; feat_sets }

let map_distinct_union equal m1 m2 =
  try
    Some (
        Metavar.Map.union
          (fun _ v1 v2 ->
            if equal v1 v2 then
              Some v1
            else
              raise (Invalid_argument "map_distinct_union"))
          m1 m2
      )
  with
    Invalid_argument _ -> None

let merge aff1 aff2 =
  let (>>=) = BatOption.bind in
  map_distinct_union Var.equal aff1.vars aff2.vars >>= fun vars ->
  map_distinct_union Feat.equal aff1.feats aff2.feats >>= fun feats ->
  map_distinct_union Kind.equal aff1.kinds aff2.kinds >>= fun kinds ->
  map_distinct_union Feat.Set.equal aff1.feat_sets aff2.feat_sets >>= fun feat_sets ->
  Some { vars ; feats ; kinds ; feat_sets }

let compatible aff1 aff2 =
  merge aff1 aff2 <> None
