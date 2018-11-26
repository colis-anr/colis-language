type t

val fresh : unit -> t
val fresh2 : unit -> t * t
val fresh3 : unit -> t * t * t
val fresh4 : unit -> t * t * t * t
val fresh5 : unit -> t * t * t * t * t

val compare : t -> t -> int
val equal : t -> t -> bool

module Map : Map.S with type key = t
