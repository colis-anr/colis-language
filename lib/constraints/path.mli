type t

val empty : t

val to_list : t -> Feat.t list

val split_first : t -> Feat.t * t
val split_last : t -> t * Feat.t

val from_string : string -> t
