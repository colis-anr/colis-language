open Constraints_common

type t

val var : t -> Metavar.t -> Var.t
val feat : t -> Metavar.t -> Feat.t
val feat_set : t -> Metavar.t -> Feat.Set.t

val empty : t

val from_lists :
  ?vars:(Metavar.t * Var.t) list ->
  ?feats:(Metavar.t * Feat.t) list ->
  ?feat_sets:(Metavar.t * Feat.Set.t) list -> unit -> t

val merge : t -> t -> t option
