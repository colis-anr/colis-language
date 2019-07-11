(** {1 Core} *)

open Constraints_common

(** {2 Information About a Variable} *)

type info
(** Type of the information we have on a variable. *)

type kind = Any | Neg of Kind.t list | Pos of Kind.t
val get_kind : info -> kind

type feat = DontKnow | Absent | Present of IVar.t | Maybe of IVar.t list
val get_feat : Feat.t -> info -> feat option
val set_feat : Feat.t -> feat -> info -> info
val del_feat : Feat.t -> info -> info

val has_fen : info -> bool

(** {2 Main Structure} *)

type var

type t
(** Type of an irreducible existential clause. *)

val get_info : var -> t -> info

(** {2 Global Helpers} *)

val for_all_similar :
  ?guard:(Feat.Set.t -> bool) ->
  (info -> info) ->
  info ->
  t -> t
(** [for_all_similar ~guard upd info] takes a clause and applies the [upd]
    function to all the info records of variables that are similar to the given
    info (including it) when the guard accepts the similarity indice. *)
