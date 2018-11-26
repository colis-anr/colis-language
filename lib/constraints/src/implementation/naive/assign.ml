open Constraints_common

type t =
  { vars : Var.t Metavar.Map.t ;
    feats : Feat.t Metavar.Map.t ;
    feat_sets : Feat.Set.t Metavar.Map.t }

let var aff m = Metavar.Map.find m aff.vars
let feat aff m = Metavar.Map.find m aff.feats
let feat_set aff m = Metavar.Map.find m aff.feat_sets

let empty =
  { vars = Metavar.Map.empty ;
    feats = Metavar.Map.empty ;
    feat_sets = Metavar.Map.empty }

let from_lists ?(vars=[]) ?(feats=[]) ?(feat_sets=[]) () =
  let rec map_add_list l m =
    match l with
    | [] -> m
    | (hk, hv) :: t -> map_add_list t (Metavar.Map.add hk hv m)
  in
  { vars = map_add_list vars Metavar.Map.empty ;
    feats = map_add_list feats Metavar.Map.empty ;
    feat_sets = map_add_list feat_sets Metavar.Map.empty }

let (>>=) = OptionMonad.(>>=)

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
  map_distinct_union Var.equal aff1.vars aff2.vars >>= fun vars ->
  map_distinct_union Feat.equal aff1.feats aff2.feats >>= fun feats ->
  map_distinct_union Feat.Set.equal aff1.feat_sets aff2.feat_sets >>= fun feat_sets ->
  Some { vars ; feats ; feat_sets }

let compatible aff1 aff2 =
  merge aff1 aff2 <> None
