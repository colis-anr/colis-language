type comp =
  | Up
  | Here
  | Down of Feat.t

type rel = comp list

type normal = Feat.t list

type t =
  | Abs of rel
  | Rel of rel

val empty_rel : rel

val split_first_rel : rel -> (comp * rel) option
val split_last_rel : rel -> (rel * comp) option
val split_last : t -> (t * comp) option

val from_string : string -> t
val strip_trailing_slashes : string -> string

val to_string : t -> string
val rel_to_string : ?abs:bool -> rel -> string
val normal_to_string : normal -> string

val pp : Format.formatter -> t -> unit

val rel : t -> rel

val concat : normal -> t -> rel
(** [concat cwd q] concatenates [cwd] and [q]. If [q] is absolute, [cwd] is
    ignored. The result is an absolute path. *)

val check_normal : t -> normal
(** Checks that the given path is absolute and does not contain [.] or [..].
    Returns the corresponding [normal] path. Or raises [Invalid_argument]. *)

val normalize : ?cwd:normal -> t -> normal
(** Normalizes syntactically the given path, starting at the given CWD. If the
    path is absolute, the CWD is ignored. *)

val check_prefix : normal -> normal -> bool
(** Returns true iff the first path is a (strict) prefix of the second path.
 *  Both path are normalized.
 *)
