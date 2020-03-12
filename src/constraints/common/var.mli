type t
[@@deriving yojson]

val fresh : unit -> t

val compare : t -> t -> int
val equal : t -> t -> bool

val to_string : t -> string
val pp : Format.formatter -> t -> unit

module Set : Derivable.SetS with type elt = t
module Map : Derivable.MapS with type key = t
