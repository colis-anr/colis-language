(** Symbolic execution of `test` utility *)

val name : string
val interpret : bracket:bool -> SymbolicUtility.context -> SymbolicUtility.utility
