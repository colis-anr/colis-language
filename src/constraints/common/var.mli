type t

val fresh : ?hint:string -> unit -> t

val hint : t -> string

val compare : t -> t -> int
val equal : t -> t -> bool

val to_string : t -> string
val pp : Format.formatter -> t -> unit

module Set : Derivable.SetS with type elt = t
module Map : Derivable.MapS with type key = t

val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) Result.result
