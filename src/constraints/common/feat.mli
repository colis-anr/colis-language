type t

val from_string : string -> t
val to_string : t -> string

val compare : t -> t -> int
val equal : t -> t -> bool

val pp : Format.formatter -> t -> unit

module Set : Derivable.SetS with type elt = t
module Map : Derivable.MapS with type key = t
