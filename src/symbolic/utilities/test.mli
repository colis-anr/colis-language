open SymbolicUtility.ConstraintsCompatibility

val name : string
val interprete : context -> utility

module Bracket : sig
  val name : string
  val interprete : context -> utility
end
