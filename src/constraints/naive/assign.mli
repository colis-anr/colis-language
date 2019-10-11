open Colis_constraints_common

type t

val var : t -> Metavar.t -> Var.t
val feat : t -> Metavar.t -> Feat.t
val kind : t -> Metavar.t -> Kind.t
val feat_set : t -> Metavar.t -> Feat.Set.t

val empty : t

val from_lists :
  ?vars:(Metavar.t * Var.t) list ->
  ?feats:(Metavar.t * Feat.t) list ->
  ?kinds:(Metavar.t * Kind.t) list ->
  ?feat_sets:(Metavar.t * Feat.Set.t) list ->
  unit -> t option

val merge : t -> t -> t option

val compatible : t -> t -> bool
