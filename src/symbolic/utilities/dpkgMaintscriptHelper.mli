(** Symbolic execution of dpkg-maintscript-helper *)
open SymbolicUtility.ConstraintsCompatibility

val name : string
val interprete : utility_context -> utility
