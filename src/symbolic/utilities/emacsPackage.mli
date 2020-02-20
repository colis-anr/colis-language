(** Symbolic execution of emacs-package-[install|remove] *)
open SymbolicUtility.ConstraintsCompatibility

module Install : sig
  val name : string
  val interprete : context -> utility
end

module Remove : sig
  val name : string
  val interprete : context -> utility
end


