open Constraints
open SymbolicInterpreter__State

(** A utility takes a list of string arguments and transforms a symbolic states into a
    list of symbol states with boolean results *)
type utility =
  string list -> state -> (state * bool) list

type case

val success_case: descr:string -> Clause.t -> case

val error_case: descr:string -> ?error_message:string -> Clause.t -> case

val failure: ?error_message:string -> unit -> case list

type specifications = cwd:Path.t -> root:Var.t -> root':Var.t -> case list

val under_specifications : specifications -> state -> (state * bool) list

