(** {1 Core} *)

open Constraints_common

(** {2 Main Structure} *)

type var
(** Type of the variables. *)

type info
(** Type of the information we have on a variable. *)

type t
(** Type of an irreducible existential clause. *)

val empty : t
(** Empty existential clause. That is, "true". *)

(** {2 Variables and information} *)

val hash : var -> int

val equal : var -> var -> t -> bool

val identify : var -> var -> (info -> info -> info) ->  t -> t
(** Identify two variables. After that, they are "equal" in the structure. The function talking about infos is here to tell to the structure how to merge the info of the two variables. *)

val fresh_var : t -> (var * t)

val internalise : Constraints_common.Var.t -> t -> (var * t)

val externalise : var -> t -> Constraints_common.Var.t list
(** Returns the (possibly empty) list of external variables mapping to that
    particular (not up to equality) internal variable. *)

val quantify_over : Constraints_common.Var.t -> t -> t

val make_initial : t -> t

val is_initial : var -> t -> bool

val get_info : var -> t -> info

val set_info : var -> t -> info -> t

val update_info : var -> t -> (info -> info) -> t

val iter : (var -> info -> unit) -> t -> unit

val iter_equalities : (var -> var -> unit) -> t -> unit

(** {2 Helpers} *)

(** {3 kinds} *)

type kind = Any | Neg of Kind.t list | Pos of Kind.t

val get_kind : info -> kind

val set_kind : kind -> info -> info

(** {3 feats} *)

type feat = DontKnow | Absent | Present of var | Maybe of var list

val get_feat : Feat.t -> info -> feat option

val iter_feats : (Feat.t -> feat -> unit) -> info -> unit
val fold_feats : (Feat.t -> feat -> 'a -> 'a) -> 'a -> info -> 'a

val for_all_feats : (Feat.t -> feat -> bool) -> info -> bool

val set_feat : Feat.t -> feat -> info -> info

val set_feat_if_none : Feat.t -> feat -> info -> info

val remove_feat : Feat.t -> info -> info

val remove_feats : (Feat.t -> bool) -> info -> info

val remove_all_feats : info -> info

(** {3 fens} *)

val has_fen : info -> bool

val set_fen : info -> info

(** {3 sims} *)

val update_sim : var -> (Feat.Set.t option -> Feat.Set.t) -> t -> info -> info
(** [update_sim y f c info] updates the sims in [info] by calling [f None] if
    there is no sim for [y] or [Some gs] if there is a sim [~gs y]. *)

val iter_sims : (Feat.Set.t -> var -> unit) -> info -> unit
val fold_sims : (Feat.Set.t -> var -> 'a -> 'a) -> 'a -> info -> 'a

val update_info_for_all_similarities :
  (Feat.Set.t -> var -> info -> info) ->
  var -> info -> t -> t
(** [update_info_for_all_similarities upd x info] takes a clause and
    applies the [upd] function to all the info records of variables that are
    similar to the given info (including it). *)

(** {3 nfens} *)

val remove_nfens : info -> info
(** Remove all nfens in the given [info]. *)

val not_implemented_nfens : info -> unit
(* Raise [NotImplemented "nfens"] if there are nfens in the given [info].

   FIXME: should not be required if everything is correctly implemented. They
   are here to denote places where work has to be done to support nfens. *)

(** {3 nsims} *)

val remove_nsims : var -> t -> t
(** Remove all nsims in the given [info]. *)

val not_implemented_nsims : info -> unit
(* Raise [NotImplemented "nsims"] if there are nsims in the given [info].

   FIXME: should not be required if everything is correctly implemented. They
   are here to denote places where work has to be done to support nsims. *)

(** {2 Should Disappear}

    FIXME *)

val not_implemented : string -> 'a

val pp_debug : Format.formatter -> t -> unit
