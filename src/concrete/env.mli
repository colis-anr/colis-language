(** Identifier environments with default value. *)

type 'a env
val empty : 'a -> 'a env
val get : 'a env -> string -> 'a
val set : 'a env -> string -> 'a -> 'a env
val filter : (string -> 'a -> bool) -> 'a env -> 'a env
val map : ('a -> 'b) -> 'a env -> 'b env
