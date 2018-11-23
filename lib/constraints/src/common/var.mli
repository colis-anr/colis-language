type t

val fresh :
  ?hint:string -> ?hintf:Feat.t -> ?hintv:t -> ?hintp:Path.t ->
  unit -> t

val compare : t -> t -> int
val equal : t -> t -> bool

val show : t -> string
val pp : Format.formatter -> t -> unit

module Set : (Set.S with type elt = t)
module Map : (Map.S with type key = t)
