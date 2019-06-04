type t

val eq : t -> t -> bool
val lt : t -> t -> bool

val pp : Format.formatter -> t -> unit

(** {2 Maps from internal variables} *)

type 'a map

val empty_map : 'a map

val equal : 'a map -> t -> t -> bool
val set_equal : 'a map -> t -> t -> ('a -> 'a -> 'a) -> 'a map

val iter : 'a map -> (t -> 'a -> unit) -> unit
val iter_sons : 'a map -> (t -> t -> unit) -> unit

val get : 'a map -> t -> 'a
val set : 'a map -> t -> 'a -> 'a map

(** {2 Maps from global variables to internal variable} *)

type globals

val empty_globals : globals

val iter_globals : globals -> (Constraints_common.Var.t -> t -> unit) -> unit

val get_global : globals -> t -> Constraints_common.Var.t option

val internalise :
  Constraints_common.Var.t ->
  globals ->
  'a map -> 'a ->
  t * globals * 'a map

val quantify_over : Constraints_common.Var.t -> globals -> globals
