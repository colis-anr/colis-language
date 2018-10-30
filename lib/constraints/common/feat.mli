type t

val from_string : string -> t
val to_string : t -> string
val pp_print : Format.formatter -> t -> unit

module Set : (Set.S with type elt = t)
