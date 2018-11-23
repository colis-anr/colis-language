open Constraints_common

type t = (* invariant: always sorted *)
  { vars : (Metavar.t * Var.t) list ;
    feats : (Metavar.t * Feat.t) list ;
    feat_sets : (Metavar.t * Feat.Set.t) list }

let var aff m = List.assoc m aff.vars
let feat aff m = List.assoc m aff.feats
let feat_set aff m = List.assoc m aff.feat_sets

let empty = { vars = [] ; feats = [] ; feat_sets = [] }

let sort l = List.sort (fun (m, _) (n, _) -> Metavar.compare m n) l

let from_lists ?(vars=[]) ?(feats=[]) ?(feat_sets=[]) () =
  { vars = sort vars ;
    feats = sort feats ;
    feat_sets = sort feat_sets }

let (>>=) = OptionMonad.(>>=)

let simplify equal subaff =
  let open OptionMonad in
  let rec simplify = function
    | [] -> Some []
    | [a] -> Some [a]
    | (m, x) :: (n, y) :: subaff ->
       if Metavar.equal m n then
         if equal x y then
           simplify ((n, y) :: subaff)
         else
           None
       else
         simplify ((n, y) :: subaff) >>= fun subaffect ->
         Some ((m, x) :: subaffect)
  in
  simplify subaff

let merge aff1 aff2 =
  (aff1.vars @ aff2.vars) |> sort |> simplify Var.equal >>= fun vars ->
  (aff1.feats @ aff2.feats) |> sort |> simplify Feat.equal >>= fun feats ->
  (aff1.feat_sets @ aff2.feat_sets) |> sort |> simplify Feat.Set.equal >>= fun feat_sets ->
  Some { vars ; feats ; feat_sets }
