(** Symbolic execution of dpkg-maintscript-helper *)
open SymbolicUtility.Symbolic

val name : string
val interprete : context -> utility
