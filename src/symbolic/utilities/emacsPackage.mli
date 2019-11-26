(** Symbolic execution of emacs-package-[install|remove] *)

module Install : sig
  val name : string
  val interprete : SymbolicUtility.context -> SymbolicUtility.utility
end

module Remove : sig
  val name : string
  val interprete : SymbolicUtility.context -> SymbolicUtility.utility
end


