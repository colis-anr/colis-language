open Constraints
open SymbolicInterpreter__Filesystem
open SymbolicInterpreter__State

(** {2 Utilities} *)

(** A utility takes a list of string arguments and transforms a symbolic states into a
   list of symbol states with boolean results *)
type utility =
  string list -> state -> (state * bool) list

(** {2 Specifications}

    Depending on the arguments (after consuming them) the state transformation [state ->
    (state * bool) state] can be implemented by specifying different cases. Each case
    comprises an outcome and a clause. *)

type outcome = Success | Error

let outcome_to_bool = function
  | Success -> true
  | Error -> false

type case = {
  outcome : outcome;
  spec : Clause.t
}

let apply_case_to_state state new_root case : (state * bool) list =
  (* Add the spec to the current clause *)
  Clause.add_to_sat_conj case.spec state.filesystem.clause
  (* For each clause in the received disjunction, create the
     corresponding filesystem. *)
  |> List.fold_left
    (fun filesystems clause ->
       { root = new_root ;
         clause ;
         cwd = state.filesystem.cwd (* FIXME *) }
       :: filesystems) []
  (* Update the state with the filesystems and return the corresponding result *)
  |> List.map
    (fun filesystem ->
       { state with filesystem }, outcome_to_bool case.outcome)

type spec = Path.t -> Var.t -> Var.t -> case list

let under_specs : spec -> state -> (state * bool) list =
  fun spec state ->
    let new_root = Var.fresh ~hint:(Var.to_string state.filesystem.root) () in
    List.map
      (apply_case_to_state state new_root)
      (spec state.filesystem.cwd state.filesystem.root new_root)
    |> List.flatten
