type t

val from_string : string -> t
val to_string : t -> string

val compare : t -> t -> int
val equal : t -> t -> bool

val pp : Format.formatter -> t -> unit

module Set : sig
  include Set.S with type elt = t

  val pp_gen : open_:string -> close:string -> empty:string -> Format.formatter -> t -> unit
  val pp : Format.formatter -> t -> unit
end

module Map : sig
  include Map.S with type key = t

  val map_filter : ('a -> 'b option) -> 'a t -> 'b t
end
