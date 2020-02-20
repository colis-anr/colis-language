open Colis_constraints
open Syntax__Syntax
open Semantics__Buffers
open Semantics__Context
open Semantics__UtilityContext
open Semantics__Input
open Semantics__Result

(** Definition of a symbolic filesystem to instantiate the symbolic interpreter *)
module type FILESYSTEM = sig
  type filesystem
end

(** Definition of a case specification and its application to a filesystem to define
    utilities by specifications. *)
module type CASESPEC = sig

  (** A case specification is a function from the old root variable and the new root root
      variable to a clause. *)
  type case_spec

  (** The no-op case specification introduces an equality constraint between the old root
      and the new root. *)
  val noop : case_spec

  type filesystem

  (** Apply a case specification to a filesystem, resulting in possible multiple new
      filesystems *)
  val apply_spec : filesystem -> case_spec -> filesystem list
end

module Make (Filesystem: FILESYSTEM) : sig

  module Semantics : module type of SymbolicInterpreter__Interpreter.MakeSemantics (Filesystem)

  open Semantics

  (** {1 Interpretation of a colis program} *)

  type sym_state = {
    context : context;
    state : state;
  }

  (** [interp_program inp stas p] symbolic interpretation configured by [inp] of a program
      [p] in a list of symbolic input states [stas]. Returns the symbolic states
      representing successful, errorneous, and incomplete interpretation. *)
  val interp_program : input -> sym_state list -> program -> state list * state list * state list

  (** {1 Basics of symbolic utility} *)

  (** A utility transforms a symbolic state into a list of symbolic states
      with boolean results *)
  type utility = state -> (state * bool result) list

  (** {1 Registration and dispatch of utilities} *)

  (** A symbolic utility is comprised of a name and a function that interpretes a [utility]
      in a [context]. *)
  module type SYMBOLIC_UTILITY = sig
    val name : string
    val interprete : utility_context -> utility
  end

  (** Register a symbolic utility *)
  val register : (module SYMBOLIC_UTILITY) -> unit

  val is_registered : string -> bool

  (** A saner way to call [dispatch] (e.g. in the implementation of utilities) *)
  val call : string -> utility_context -> string list -> utility

  (** {1 Basic utilities} *)

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

  (** {1 Utility combinators} *)

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

  (** {1 Arguments Parsing} *)

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

    open CaseSpec

    (** A case in the specification is either a success, an error, or incomplete *)
    type case

    (** A success case (aka. return 0) *)
    val success_case: descr:string -> ?stdout:Stdout.t -> case_spec -> case

    (** An error case (aka return 1) *)
    val error_case: descr:string -> ?stdout:Stdout.t -> ?error_message:string -> case_spec -> case

    (** An incomplete case (unknown behaviour, cannot be symbolically executed) *)
    val incomplete_case: descr:string -> case_spec -> case

    (** Use a list of cases to specify a utility. Corresponds to a table in the
        document "Specification of UNIX Utilities "*)
    val specification_cases : case list -> utility
  end
end

(** {1 Constraints} *)

type constraints_filesystem = {
  root: Var.t;
  clause: Clause.sat_conj;
  root0: Var.t option;
}

type constraints_case_spec = Var.t -> Var.t -> Clause.t

(** Parameter for the symbolic engine using a constraint-based filesystem *)
module ConstraintsImplementation : CASESPEC
  with type filesystem = constraints_filesystem
   and type case_spec = constraints_case_spec

(** Get the name of the last path component, if any, or of the hint
    root variable otherwise. The result is useful as a hint for
    creating variables for resolving the path. *)
val last_comp_as_hint: root:Var.t -> Path.t -> string option

(** {1 Transducers} *)

type transducers_filesystem = unit (* TODO *)

type transducers_case_spec = unit (* TODO *)

(** Parameter for the symbolic engine using transducers as filesystem *)
module TransducersImplementation : CASESPEC
  with type filesystem = transducers_filesystem
   and type case_spec = transducers_case_spec

(** {1 Mixed constraints/transducers} *)

type mixed_filesystem =
  | Constraints of constraints_filesystem
  | Transducers of transducers_filesystem

type mixed_case_spec

(** Create a mixed case specification. Any missing case specs will trigger incomplete
    behaviour. *)
val mixed_case_spec :
  ?transducers:transducers_case_spec -> ?constraints:constraints_case_spec -> unit -> mixed_case_spec

module MixedImplementation : CASESPEC
  with type filesystem = mixed_filesystem
   and type case_spec = mixed_case_spec

include module type of Make (MixedImplementation)
include module type of MakeSpecifications (MixedImplementation)
val noop : mixed_case_spec
type context = utility_context = {
  cwd: Path.normal;
  env: string Env.SMap.t;
  args: string list;
}

(** Compatibility with module SymbolicUtility before functorization of the symbolic engine. *)
module ConstraintsCompatibility : sig

  include module type of Make (MixedImplementation)
  include module type of MakeSpecifications (MixedImplementation)

  type context = utility_context = {
    cwd: Path.normal;
    env: string Env.SMap.t;
    args: string list;
  }

  type filesystem = constraints_filesystem

  val success_case: descr:string -> ?stdout:Stdout.t -> constraints_case_spec -> case
  val error_case: descr:string -> ?stdout:Stdout.t -> ?error_message:string -> constraints_case_spec -> case
  val incomplete_case: descr:string -> constraints_case_spec -> case

  val noop : constraints_case_spec

  (** Get the name of the last path component, if any, or of the hint
      root variable otherwise. The result is useful as a hint for
      creating variables for resolving the path. *)
  val last_comp_as_hint: root:Var.t -> Path.t -> string option
end
