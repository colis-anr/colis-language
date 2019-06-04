open Constraints
open SymbolicInterpreter__Semantics
open Semantics__Buffers

(** A utility transforms a symbolic state into a list of symbolic states
    with boolean results *)
type utility = state -> (state * bool) list

(** {2 Dispatch} *)

(** The concrete evaluation context. It contains the fields from
    Colis.Semantics.Context.context that are relevant to the utilities **)
type context = {
  args: string list; (** Command arguments *)
  cwd: Path.normal; (** Current working directory *)
  env: string Env.IdMap.t; (** Variable environment *)
}

(** Entry-point for the interpretation of symbolic utilties *)
val dispatch : name:string -> context -> utility

(** A saner way to call [dispatch] (e.g. in the implementation of utilities) *)
val call : string -> context -> string list -> utility

(** {2 Registration} *)

module type SYMBOLIC_UTILITY = sig
  val name : string
  val interprete : context -> utility
end

(** Register a symbolic utility *)
val register : (module SYMBOLIC_UTILITY) -> unit

(** {1 Specifications} *)

(** A case in the specification is either a success or an error *)
type case

(** A success case *)
val success_case: descr:string -> ?stdout:Stdout.t -> Clause.t -> case

(** An error case *)
val error_case: descr:string -> ?stdout:Stdout.t -> ?error_message:string -> Clause.t -> case

(** A singleton error case with optional error message *)
val failure: ?error_message:string -> unit -> case list

(** The specifications of a utility are a list of cases that depend on the current working
   directory, the old root variable, and a new root variable *)
type specifications = root:Var.t -> root':Var.t -> case list

(** Use specifications to define a utility *)
val under_specifications : specifications -> utility

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

(** compose_non_strict [u1] [u2] yields the utility that behaves like
    [u1]; [u2] in non-strict mode, that is the error code of [u1] is
    ignored *)
val compose_non_strict : utility -> utility -> utility

(** compose_strict [u1] [u2] yields the utility that behaves like
    [u1]; [u2] in strict mode, that is if [u1] fails then the composition
    fails and [u2]  is not executed    *)
val compose_strict : utility -> utility -> utility

(** {1 Auxiliaries} *)

(** Get the name of the last path component, if any, or of the hint
    root variable otherwise. The result is useful as a hint for
    creating variables for resolving the path. *)
val last_comp_as_hint: root:Var.t -> Path.t -> string option

(** Error utility with optional message *)
val error : ?msg:string -> unit -> utility

(** Wrapper around [error] in case of unknown utility. *)
val unknown_utility : ?msg:string -> name:string -> unit -> utility

(** Wrapper around [error] in case of unknown argument. *)
val unknown_argument : ?msg:string -> name:string -> arg:string -> unit -> utility

(** Print to stdout but mark the line with [UTL] *)
val print_utility_trace : string -> state -> state

(**/**)

(** A wrapper of [dispatch] for use in the Why3 driver *)
val dispatch' : (Path.normal * string Env.IdMap.t * string list) -> string -> state -> (state * bool) BatSet.t
