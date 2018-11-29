type t

val fresh : ?hint:string -> unit -> t

val hint : t -> string

val compare : t -> t -> int
val equal : t -> t -> bool

val to_string : t -> string
val pp : Format.formatter -> t -> unit

module Set : sig
  include Set.S with type elt = t

  val pp : Format.formatter -> t -> unit
end

module Map : (Map.S with type key = t)
