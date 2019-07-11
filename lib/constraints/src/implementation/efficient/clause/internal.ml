open Constraints_common

exception NotImplemented of string
let not_implemented s = raise (NotImplemented s)

(* {2 Absence} *)

let abs x f c =
  let info = Core.get_info x c in
  match Core.get_kind info with

  | Pos k when k <> Kind.Dir ->
    (* S-Abs-Kind *)
    Dnf.single c

  | Neg ks when List.mem Kind.Dir ks ->
    (* S-Abs-NDir *)
    Dnf.single c

  | _ ->
    match Core.get_feat f info with

    | None when Core.has_fen info ->
      (* P-Abs + S-Abs-Fen*)
      Core.(
        update_info_for_all_similarities
        ~guard:(fun fs -> not (Feat.Set.mem f fs))
        (del_feat f)
        info c
      ) |> Dnf.single

    | None | Some DontKnow | Some (Maybe _) | Some Absent ->
      (* (S-Maybe-Abs) + P-Abs *)
      Core.(
        update_info_for_all_similarities
          ~guard:(fun fs -> not (Feat.Set.mem f fs))
          (set_feat f Absent)
          info c
      ) |> Dnf.single

    | Some (Present _) ->
      (* C-Abs-Feat *)
      Dnf.empty
