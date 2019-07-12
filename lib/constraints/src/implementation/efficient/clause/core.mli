(** {1 Core} *)

open Constraints_common

(** {2 Variable} *)

type var

val fresh_var : unit -> var

(** {2 Information About a Variable} *)

type info
(** Type of the information we have on a variable. *)

type kind = Any | Neg of Kind.t list | Pos of Kind.t
val get_kind : info -> kind
val set_kind : kind -> info -> info

type feat = DontKnow | Absent | Present of var | Maybe of var list
val get_feat : Feat.t -> info -> feat option
val set_feat : Feat.t -> feat -> info -> info
val del_feat : Feat.t -> info -> info
val del_feats : info -> info

val has_fen : info -> bool

val del_nfeats : info -> info
val del_nfens : info -> info

(** {2 Main Structure} *)

type t
(** Type of an irreducible existential clause. *)

val empty : t
(** Empty existential clause. That is, "true". *)

val internalise : Constraints_common.Var.t -> t -> (var * t)

val get_info : var -> t -> info
val set_info : var -> t -> info -> t

val del_nsims : var -> t -> t

(** {2 Global Helpers} *)

val update_info_for_all_similarities :
  ?guard:(Feat.Set.t -> bool) ->
  (info -> info) ->
  info ->
  t -> t
(** [update_info_for_all_similarities ~guard upd info] takes a clause and
    applies the [upd] function to all the info records of variables that are
    similar to the given info (including it) when the guard accepts the
    similarity indice. *)
