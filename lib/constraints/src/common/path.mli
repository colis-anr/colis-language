type comp =
  | Up
  | Here
  | Down of Feat.t

type rel = comp list

val empty_rel : rel

val split_first_rel : rel -> (comp * rel) option
val split_last_rel : rel -> (rel * comp) option

type t =
  | Abs of rel
  | Rel of rel

val from_string : string -> t

val rel : t -> rel

val concat : t -> rel -> t

val normalize : t -> Feat.t list

val split_last : t -> (t * comp) option
