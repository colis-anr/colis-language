type t

val from_string : string -> t
val to_string : t -> string
val pp_print : Format.formatter -> t -> unit

val compare : t -> t -> int
val equal : t -> t -> bool

val show : t -> string
val pp : Format.formatter -> t -> unit

module Set : (Set.S with type elt = t)
module Map : (Map.S with type key = t)
