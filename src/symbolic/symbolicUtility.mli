open Colis_constraints
open Semantics__Result
open Semantics__Buffers

module type FILESYSTEM = sig
  type filesystem
end

module type CASESPEC = sig

  (** A case specification is a function from the old root variable and the new root root
      variable to a clause. *)
  type case_spec

  (** The no-op case specification introduces an equality constraint between the old root
      and the new root. *)
  val noop : case_spec

  type filesystem
  val apply_spec : filesystem -> case_spec -> filesystem list
end

module Make (Filesystem: FILESYSTEM) : sig

  module Semantics : module type of SymbolicInterpreter__Interpreter.MakeSemantics (Filesystem)
  open Semantics

  type sym_state = {
    context : Semantics__Context.context;
    state : Semantics.state;
  }

  val interp_program :
    Colis__Semantics__Input.input ->
    sym_state list ->
    Colis__Syntax__Syntax.program ->
    Semantics.state list * Semantics.state list * Semantics.state list


  (** {1 Basics of symbolic utility} *)

  (** A utility transforms a symbolic state into a list of symbolic states
      with boolean results *)
  type utility = state -> (state * bool result) list

  (** A symbolic utility is comprised of a name and a function that interpretes a [utility]
      in a [context]. *)
  module type SYMBOLIC_UTILITY = sig
    val name : string
    val interprete : utility_context -> utility
  end

  (** {1 Registration} *)

  (** Register a symbolic utility *)
  val register : (module SYMBOLIC_UTILITY) -> unit

  val is_registered : string -> bool

  (** {1 Dispatch} *)

  (** A saner way to call [dispatch] (e.g. in the implementation of utilities) *)
  val call : string -> utility_context -> string list -> utility

  (** {1 Combinators} *)

  (** [choice u1 u2]  yields the utility that non-deterministacillay
      behaves like [u1] or [u2].  *)
  val choice : utility -> utility -> utility

  (** [return b] yields the utility that does not change its state, and
      succeeds if and only if [b] is [true] *)
  val return : bool -> utility

  (** [if_then_else u1 u2 u3] yields the utility that behaves like
      if [u1]  then [u2] else [u3] fi *)
  val if_then_else : utility -> utility -> utility -> utility

  (** [if_then u1 u2] yields the utility that behaves like
      if [u1] then [u2] fi *)
  val if_then : utility -> utility -> utility

  (** [uneg u] yields the utility that behaves like [u] but inverts its result *)
  val uneg : utility -> utility

  (** [uand u1 u2] yields the utility that behaves like [u1 && u2] in a NON-LAZY manner *)
  val uand : utility -> utility -> utility

  (** [uor u1 u2] yields the utility that behaves like [u1 || u2] in a NON-LAZY manner *)
  val uor : utility -> utility -> utility

  (** multiple_times [what] [args] executes [what] on every argument in
      [args]. It does not stop if one of these executions fails but the
      global utility fails in the case. *)
  val multiple_times : ('a -> utility) -> 'a list -> utility

  (** compose_non_strict [u1] [u2] yields the utility that behaves like
      [u1]; [u2] in non-strict mode, that is the error code of [u1] is
      ignored *)
  val compose_non_strict : utility -> utility -> utility

  (** compose_strict [u1] [u2] yields the utility that behaves like
      [u1]; [u2] in strict mode, that is if [u1] fails then the composition
      fails and [u2]  is not executed    *)
  val compose_strict : utility -> utility -> utility

  (** {1 Auxiliaries} *)

  (** Error utility *)
  val error : utility:string -> string -> utility

  (** Unsupported stuff in a known utility. *)
  val incomplete : utility:string -> string -> utility

  (* Unknown-unknown behaviour *)
  val unknown : utility:string -> string -> utility

  (** {2 Printing} *)

  (** Print to stdout and log *)
  val print_stdout : newline:bool -> string -> state -> state

  (** Print message as utility trace to log if it is not empty (marked as [UTL]) *)
  val print_utility_trace : string -> state -> state

  (** {2 Arguments Parsing} *)

  val cmdliner_eval_utility :
    utility:string ->
    ?empty_pos_args:bool ->
    (utility_context -> string list -> utility) Cmdliner.Term.t ->
    utility_context -> utility
  (** A wrapper around [Cmdliner.Term.eval] for utilities. [utility] is the name
      of the utility. [empty_pos_args] describe whether empty positional arguments
      lists are accepted or not (refused by default). *)

  (** {1 Specifications of a symbolic utility} *)

  module MakeSpecifications (CaseSpec: CASESPEC with type filesystem = Filesystem.filesystem) : sig

    (** A case in the specification is either a success or an error *)
    type case

    (** A success case (aka. return 0) *)
    val success_case: descr:string -> ?stdout:Stdout.t -> CaseSpec.case_spec -> case

    (** An error case (aka return 1) *)
    val error_case: descr:string -> ?stdout:Stdout.t -> ?error_message:string -> CaseSpec.case_spec -> case

    (** An incomplete case (unknown behaviour, cannot be symbolically executed) *)
    val incomplete_case: descr:string -> CaseSpec.case_spec -> case

    (** Use a list of cases to specify a utility. Corresponds to a table in the
        document "Specification of UNIX Utilities "*)
    val specification_cases : case list -> utility
  end
end

module SymbolicFilesystem : sig
  type filesystem = {
    root: Var.t;
    clause: Clause.sat_conj;
    root0: Var.t option;
  }
end

module SymbolicCaseSpec : CASESPEC with
  type filesystem = SymbolicFilesystem.filesystem and
  type case_spec = Var.t -> Var.t -> Clause.t

(* Compatibility with previous SymbolicUtility *)
module Symbolic : sig

  include module type of Make (SymbolicFilesystem)

  include module type of MakeSpecifications (SymbolicCaseSpec)

  type context = Semantics.utility_context = {
    cwd: Colis_constraints.Path.normal;
    env: string Env.SMap.t;
    args: string list;
  }

  val noop : SymbolicCaseSpec.case_spec

  (** Get the name of the last path component, if any, or of the hint
      root variable otherwise. The result is useful as a hint for
      creating variables for resolving the path. *)
  val last_comp_as_hint: root:Var.t -> Path.t -> string option
end
