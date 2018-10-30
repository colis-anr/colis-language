type component =
  | Up
  | Here
  | Down of Feat.t

type t

val empty : t

val to_list : t -> component list

val split_first : t -> component * t
val split_last : t -> t * component

val from_string : string -> t

val normalize_syntactically : t -> Feat.t list
