open Constraints
open Semantics__Buffers
open SymbolicInterpreter__State

(** A utility transforms a symbolic states into a list of symbol states with boolean
   results *)
type utility = state -> (bool state_result) list

(** A case in the specification is either a success or an error *)
type case

(** A success case **)
val success_case: descr:string -> ?stdout:Stdout.t -> Clause.t -> case

(** An error case **)
val error_case: descr:string -> ?stdout:Stdout.t -> Clause.t -> case

(** A failure case **)
val failure_case: descr:string -> ?stdout:Stdout.t -> Clause.t -> case

(** The specifications of a utility are a list of cases that depend on the current working
   directory, the old root variable, and a new root variable *)
type specifications = cwd:Path.t -> root:Var.t -> root':Var.t -> case list

(** Use specifications to define a utility *)
val under_specifications : specifications -> utility
