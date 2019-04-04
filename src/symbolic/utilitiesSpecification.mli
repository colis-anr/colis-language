open Constraints
open SymbolicInterpreter__State
open Semantics__Buffers

(** A utility transforms a symbolic state into a list of symbolic states
    with boolean results *)
type utility = state -> (state * bool) list

(** [choice u1 u2]  yields the utility that non-deterministacillay
    behaves like [u1] or [u2].  *)
val choice : utility -> utility -> utility

(** [return b] yields the utility that does not change its state, and
    succeeds if and only if [b] is [true] *)
val return : bool -> utility

val print_utility_trace : string -> state -> state

(** A case in the specification is either a success or an error *)
type case

(** A success case **)
val success_case: descr:string -> ?stdout:Stdout.t -> Clause.t -> case

(** An error case **)
val error_case: descr:string -> ?stdout:Stdout.t -> ?error_message:string -> Clause.t -> case

(** A singleton error case with optional error message *)
val failure: ?error_message:string -> unit -> case list

(** The specifications of a utility are a list of cases that depend on the current working
   directory, the old root variable, and a new root variable *)
type specifications = cwd:Path.t -> root:Var.t -> root':Var.t -> case list

(** Use specifications to define a utility *)
val under_specifications : specifications -> utility
