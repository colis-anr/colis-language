open SymbolicUtility.Symbolic

val name : string
val interprete : context -> utility

module Silent : sig
  val name : string
  val interprete : context -> utility
end
