type t

val empty : t

val split_last : t -> t * Feat.t

val from_string : string -> t
