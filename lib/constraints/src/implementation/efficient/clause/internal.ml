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

(** {2 Kind} *)

let kind x k c =
  let info = Core.get_info x c in
  match Core.get_kind info with

  | Pos l when k = l ->
    Dnf.single c

  | Pos _ ->
    (* C-Kind-Kind *)
    Dnf.empty

  | Neg ls when List.mem k ls ->
    (* C-Kind-NKind *)
    Dnf.empty

  | _ ->
    (* (S-NKind-Kind) *)
    match k with

    | Dir ->
      Core.(
        info
        |> set_kind (Pos Dir)
        |> set_info x c
        |> Dnf.single
      )

    | _ ->
      Core.(
        info
        |> del_feats     (* S-Abs-Kind, S-Maybe-Kind *)
        |> del_nfeats    (* S-NFeat-Kind *)
        |> del_nfens     (* S-NFen-Kind *)
        |> set_info x c
        |> del_nsims x   (* S-NSim-Kind *)
        |> Dnf.single
      )

let dir x = kind x Kind.Dir
