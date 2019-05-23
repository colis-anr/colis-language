type t

(** {2 Maps from internal variables} *)

type 'a map

val empty_map : 'a map

val equal : 'a map -> t -> t -> bool
val set_equal : 'a map -> t -> t -> ('a -> 'a -> 'a) -> 'a map

val iter : 'a map -> (t -> 'a -> unit) -> unit

val get : 'a map -> t -> 'a
val set : 'a map -> t -> 'a -> 'a map

(** {2 Maps from global variables to internal variable} *)

type globals

val empty_globals : globals

val internalise : Constraints_common.Var.t -> globals -> t * globals

val quantify_over : Constraints_common.Var.t -> globals -> globals
