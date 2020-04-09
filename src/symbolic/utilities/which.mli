open SymbolicUtility.ConstraintsCompatibility

val name : string
val interprete : utility_context -> utility

module Silent : sig
  val name : string
  val interprete : utility_context -> utility
end
