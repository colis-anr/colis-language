type t = Var.Set.t * Literal.Set.t

val equal : t -> t -> bool
val compare : t -> t -> int

val pp : Format.formatter -> t -> unit
val pp_as_dot : name:string -> Format.formatter -> t -> unit
