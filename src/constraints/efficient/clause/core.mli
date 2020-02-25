(** {1 Core} *)

open Colis_constraints_common

type var
(** Type of the variables. *)

module VarSet : Set.S with type elt = var

(** {2 Information} *)

module Info : sig
  type t

  val is_shadow : t -> bool
  val update_shadow : t -> t

  type kind = Any | Neg of Kind.t list | Pos of Kind.t
  val get_kind : t -> kind
  val set_kind : kind -> t -> t

  type feat = DontKnow | Absent | Present of var | Maybe of var list
  val get_feat : Feat.t -> t -> feat option
  val iter_feats : (Feat.t -> feat -> unit) -> t -> unit
  val fold_feats : (Feat.t -> feat -> 'a -> 'a) -> 'a -> t -> 'a
  val for_all_feats : (Feat.t -> feat -> bool) -> t -> bool
  val set_feat : Feat.t -> feat -> t -> t
  val set_feat_if_none : Feat.t -> feat -> t -> t
  val remove_feat : Feat.t -> t -> t
  val remove_feats : (Feat.t -> bool) -> t -> t
  val remove_all_feats : t -> t

  val has_fen : t -> bool
  val set_fen : t -> t

  val iter_sims : (Feat.Set.t -> var -> unit) -> t -> unit
  val fold_sims : (Feat.Set.t -> var -> 'a -> 'a) -> 'a -> t -> 'a

  val remove_nfens : t -> t
  (** Remove all nfens in the given [info]. *)

  val not_implemented_nfens : t -> unit
  (* Raise [NotImplemented "nfens"] if there are nfens in the given [info].

     FIXME: should not be required if everything is correctly implemented. They
     are here to denote places where work has to be done to support nfens. *)

  val not_implemented_nsims : t -> unit
  (* Raise [NotImplemented "nsims"] if there are nsims in the given [info].

     FIXME: should not be required if everything is correctly implemented. They
     are here to denote places where work has to be done to support nsims. *)
end

(** {2 Main Structure} *)

type t [@@deriving yojson]
(** Type of an irreducible existential clause. *)

val empty : t
(** Empty existential clause. That is, "true". *)

(** {2 Variables and information} *)

val hash : var -> int

val equal : var -> var -> t -> bool

val syntactic_compare : var -> var -> int
(** Comparison not in the structure but syntactically. Implies equality in the
    structure. *)

val identify : var -> var -> (Info.t -> Info.t -> Info.t) -> t -> t
(** Identify two variables. After that, they are "equal" in the structure. The function talking about infos is here to tell to the structure how to merge the info of the two variables. *)

val fresh_var : t -> (var * t)

val internalise : Colis_constraints_common.Var.t -> t -> (var * t)

val externalise : var -> t -> Colis_constraints_common.Var.t list
(** Returns the (possibly empty) list of external variables mapping to that
    particular (not up to equality) internal variable. *)

val fold_globals : (var -> 'a -> 'a) -> t -> 'a -> 'a

val quantify_over : Colis_constraints_common.Var.t -> t -> t

val is_shadow : var -> t -> bool

val get_info : var -> t -> Info.t
(** [get_info x c] returns the information associated with variable [x] in
   clause [c], after having updated the [shadow] field. *)

val get_info_no_shadow : var -> t -> Info.t
(** Same as [get_info] but does not update the [shadow] field. *)

val set_info : var -> t -> Info.t -> t

val update_info : var -> t -> (Info.t -> Info.t) -> t
(** Equivalent to [get_info x c |> f |> set_info x c]. *)

val filter_infos : (var -> bool) -> t -> t

val fold_infos : (var -> Info.t -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold_infos f c e] computes [f] in order for all variables and associated
   informations in the all clause [c], passing the result to the next
   computation. This does not update the [shadow] field. *)

val iter_infos : (var -> Info.t -> unit) -> t -> unit
(* Iters on all the variables and associated informations, without updating the
   [shadow] field. *)

val iter_equalities : (var -> var -> unit) -> t -> unit

(** {3 sims} *)

val remove_sim : var -> t -> Info.t -> Info.t
(** [remove_sim y c info] removes all references to variables equal to [y] (wrt.
    structure [c]) in [info]. *)

val update_sim : var -> (Feat.Set.t option -> Feat.Set.t) -> t -> Info.t -> Info.t
(** [update_sim y f c info] updates the sims in [info] by calling [f None] if
    there is no sim for [y] or [Some gs] if there is a sim [~gs y]. *)

val update_info_for_all_similarities :
  (Feat.Set.t -> var -> Info.t -> Info.t) ->
  var -> Info.t -> t -> t
(** [update_info_for_all_similarities upd x info] takes a clause and
    applies the [upd] function to all the info records of variables that are
    similar to the given info (including it). *)

(** {3 nsims} *)

val remove_nsims : var -> t -> t
(** Remove all nsims in the given [info]. *)

(** {2 Garbage Collection} *)

val simplify : t -> t
(** Removes all variables that are not accessible from the globals. *)

val with_shadow_variables : (unit -> 'a) -> 'a
(** [with_initial f] runs [f] in an environment where all new variables are
    declared to be shadow. *)

(** {2 Should Disappear} FIXME *)

val not_implemented : string -> 'a

val pp_debug : Format.formatter -> t -> unit
