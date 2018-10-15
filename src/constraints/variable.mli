
type t

val fresh : ?hint:string -> unit -> t

module Set : (Set.S with type elt = t)

module Map : (Map.S with type key = t)

val hint : t -> string option

val id : t -> int