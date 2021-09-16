open Syntax__Syntax
open Semantics__Buffers
open Semantics__Input
open Semantics__Result
open Semantics__UtilityContext

(** {1 Interfaces} *)

(** {2 Interpreter} *)
module type INTERPRETER = sig
  type filesystem

  (** {3 Interpreter state} *)

  type state = {
    filesystem: filesystem;
    stdin: Stdin.t;
    stdout: Stdout.t;
    log: Stdout.t;
  }

  (** Shortcut to make a state with empty stdin, stdout, and log *)
  val mk_state : filesystem -> state

  (** Print to stdout (and log) *)
  val print_stdout : newline:bool -> string -> state -> state

  (** Print message as utility trace to log if it is not empty (marked as [ERR]) *)
  val print_error : string -> state -> state

  (** Print message as utility trace to log if it is not empty (marked as [UTL]) *)
  val print_utility_trace : string -> state -> state

  (** Print message as utility trace to log if it is not empty (marked as [INCOMPLETE]) *)
  val print_incomplete_trace : string -> state -> state

  (** {3 Utilities} *)

  (** A utility transforms a symbolic state into a list of symbolic states
      with boolean results *)
  type utility = state -> (state * bool result) list

  (** {4 Basic utilities and behaviours}
      The symbolic interpretation of a utility call may result in different behaviours:

      {[
        Any behaviour
          ├─ Known behaviour
          │  ├─ The call was interpreted
          │  │  ├─ [success]: The call was valid and succeeded (i.e. exit 0, i.e. Ok true)
          │  │  └─ [error]: The call failed (i.e. exit >0, i.e. Ok false)
          │  └─ [incomplete]: The utility or option has not been implemented (i.e. Incomplete)
          └─ [unknown]: We don't even know if this call is an error or incomplete behaviour
      ]}

      [return true] can be used for a simple [success]. [error] adds logging to [return
      false].

      A call is valid if it corresponds to the syntax of the utility.

      The actual behaviour of [unknown] in the interpreter is determined by option
      [Option.unknown_behaviour] and may be an OCaml exception, or correspond to error
      behaviour, or incomplete behaviour. *)

  (** Error utility *)
  val error : utility:string -> string -> utility

  (** Unsupported stuff in a known utility. *)
  val incomplete : utility:string -> string -> utility

  (** Unknown-unknown behaviour **)
  val unknown : utility:string -> string -> utility

  (** {4 Registration and dispatch} *)

  (** A symbolic utility is comprised of a name and a function that interpretes a [utility]
      in a [context]. *)
  module type SYMBOLIC_UTILITY = sig
    val name : string
    val interprete : utility_context -> utility
  end

  (** Register a symbolic utility *)
  val register : (module SYMBOLIC_UTILITY) -> unit

  (** Check if a utility with [name] is registered *)
  val is_registered : name:string -> bool

  (** Dispatch a call to utility [name] in a utility context *)
  val dispatch : name:string -> utility_context -> utility

  (** A saner way to call [dispatch] (e.g. in the implementation of utilities) *)
  val call : string -> utility_context -> string list -> utility

  (** {3 Interpretation} *)

  open Semantics__Context

  (** A symbolic state combines a state with a concrete interpreter context *)
  type sym_state = {
    context : context;
    state : state;
  }

  (** [interp_program inp stas p] symbolic interpretation configured by [inp] of a program
      [p] in a list of symbolic input states [stas]. Returns the symbolic states
      representing successful, errorneous, and incomplete interpretation. *)
  val interp_program : input -> sym_state list -> program -> state list * state list * state list
end

(** {2 Utility combinators} *)
module type COMBINATORS = sig
  type state

  type utility = state -> (state * bool result) list

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

  (** {3 Arguments Parsing} *)

  val cmdliner_eval_utility :
    utility:string ->
    ?empty_pos_args:bool ->
    (utility_context -> string list -> utility) Cmdliner.Term.t ->
    utility_context -> utility
  (** A wrapper around [Cmdliner.Term.eval] for utilities. [utility] is the name
      of the utility. [empty_pos_args] describe whether empty positional arguments
      lists are accepted or not (refused by default). *)
end

module type CASE_SPEC = sig
  type filesystem
  type case_spec
  val noop : case_spec
  val apply_spec : filesystem -> case_spec -> filesystem list
end

(** {2 Specifications of a symbolic utility by specifications} *)
module type SPECIFICATIONS = sig
  type state

  type case_spec

  type utility = state -> (state * bool result) list

  (** A case in the specification is either a success, an error, or incomplete *)
  type case

  (** A success case (aka. return 0) *)
  val success_case: descr:string -> ?stdout:Stdout.t -> case_spec -> case

  (** An error case (aka return 1) *)
  val error_case: descr:string -> ?stdout:Stdout.t -> ?error_message:string -> case_spec -> case

  (** An incomplete case (unknown behaviour, cannot be symbolically executed) *)
  val incomplete_case: descr:string -> case_spec -> case

  (** Use a list of cases to specify a utility. *)
  val specification_cases : case list -> utility
end

module type FILESYSTEM = sig type filesystem end

(** {1 Makers} *)

module MakeInterpreter (Filesystem: FILESYSTEM) :
  INTERPRETER with type filesystem = Filesystem.filesystem

module MakeCombinators (Interpreter: INTERPRETER)
  : COMBINATORS with type state = Interpreter.state

module MakeSpecifications
    (Interpreter: INTERPRETER)
    (CaseSpec: CASE_SPEC with type filesystem = Interpreter.filesystem)
  : SPECIFICATIONS
    with type state = Interpreter.state
     and type case_spec = CaseSpec.case_spec

(** {1 Backends} *)

(** {2 Constraints} *)

(** Instantiation of the constraints-based backend with the interpreter and
   specifications. *)
module Constraints : sig

  open Colis_constraints

  (** A constraints-based filesystem is comprised of a root variable [root]
      and a satisfiable constraint [clause]. If the variable [root0] is
      defined, its occurrences in [clause] are not garbage-collected. *)
  type filesystem = {
    root: Var.t;
    clause: Clause.sat_conj;
    root0: Var.t option;
  }

  include INTERPRETER
    with type filesystem := filesystem

  (** A case specification is a function [fun r r' -> c] from the current root [r] and
      the new root [r] to a constraint clause [c]. *)
  include CASE_SPEC
    with type filesystem := filesystem
     and type case_spec = Var.t -> Var.t -> t

  include COMBINATORS
    with type state := state
     and type utility := utility

  include SPECIFICATIONS
    with type state := state
     and type case_spec := case_spec
     and type utility := utility

  type config = {
    prune_init_state: bool;
    (** Garbage-collect the initial root variable for a faster execution. *)
  }

  val filesystems : config -> FilesystemSpec.t -> filesystem list

  type utility_context = Semantics__UtilityContext.utility_context = {
    cwd: Colis_constraints.Path.normal;
    env: string Env.SMap.t;
    args: string list;
  }
end

(** {2 Transducers} *)

(** Instantiation of the transducers-based backend with the interpreter and
   specifications. *)
module Transducers : sig

  (** Dummies only. TODO *)
  type filesystem = unit

  include INTERPRETER
    with type filesystem := filesystem

  include CASE_SPEC
    with type filesystem := filesystem
     and type case_spec = unit

  include COMBINATORS
    with type state := state
     and type utility := utility

  include SPECIFICATIONS
    with type state := state
     and type case_spec := case_spec
     and type utility := utility

  type config = unit
  val filesystems : config -> FilesystemSpec.t -> filesystem list

  type utility_context = Semantics__UtilityContext.utility_context = {
    cwd: Colis_constraints.Path.normal;
    env: string Env.SMap.t;
    args: string list;
  }
end

(** {2 Mixed constraints/transducers}

    The mixed backend combines the constraints-based backend with the transducers-based
    backend, while sharing the infrastructure for concrete evaluation. *)
module Mixed : sig

  type filesystem =
    | Constraints of Constraints.filesystem
    | Transducers of Transducers.filesystem

  include INTERPRETER
    with type filesystem := filesystem

  include CASE_SPEC
    with type filesystem := filesystem

  (** A mixed case specification combines case specifications of the different
      backends. If the case specification is left unspecified for a backends,
      it induces incomplete behaviour for that backend independently of the
      specification case. *)
  val case_spec :
    ?transducers:Transducers.case_spec ->
    ?constraints:Constraints.case_spec ->
    unit -> case_spec

  include COMBINATORS
    with type state := state
     and type utility := utility

  include SPECIFICATIONS
    with type state := state
     and type case_spec := case_spec
     and type utility := utility

  (** Symbolically interprete a program using the constraints backend *)
  val interp_program_constraints : input -> Constraints.sym_state list -> program ->
    (Constraints.state list * Constraints.state list * Constraints.state list)

  (** Symbolically interprete a program using the transducers backend *)
  val interp_program_transducers : input -> Transducers.sym_state list -> program ->
    (Transducers.state list * Transducers.state list * Transducers.state list)

  type utility_context = Semantics__UtilityContext.utility_context = {
    cwd: Colis_constraints.Path.normal;
    env: string Env.SMap.t;
    args: string list;
  }
end

(** {2 Compatibility}

    Compatibility with module SymbolicUtility before functorization of the symbolic
    engine: Create mixed utilities with the interface of constraints-based backend. *)
module ConstraintsCompatibility : sig

  include INTERPRETER
    with type filesystem := Mixed.filesystem
     and type state := Mixed.state

  include CASE_SPEC
    with type filesystem := Mixed.filesystem
     and type case_spec := Mixed.case_spec

  include COMBINATORS
    with type state := Mixed.state
     and type utility := Mixed.utility

  include SPECIFICATIONS
    with type state := Mixed.state
     and type case_spec := Mixed.case_spec
     and type utility := Mixed.utility
     and type case := Mixed.case

  val success_case: descr:string -> ?stdout:Stdout.t -> Constraints.case_spec -> Mixed.case
  val error_case: descr:string -> ?stdout:Stdout.t -> ?error_message:string -> Constraints.case_spec -> Mixed.case
  val incomplete_case: descr:string -> Constraints.case_spec -> Mixed.case
  val noop : Constraints.case_spec

  type utility_context = Semantics__UtilityContext.utility_context = {
    cwd: Colis_constraints.Path.normal;
    env: string Env.SMap.t;
    args: string list;
  }
end
