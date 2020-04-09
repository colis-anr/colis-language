(** Symbolic execution of emacs-package-[install|remove] *)
open SymbolicUtility.ConstraintsCompatibility

module Install : sig
  val name : string
  val interprete : utility_context -> utility
end

module Remove : sig
  val name : string
  val interprete : utility_context -> utility
end


