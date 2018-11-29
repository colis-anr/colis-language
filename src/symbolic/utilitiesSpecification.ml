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

(** Create the corresponding filesystem, update the state and create corresponding
    result **)
let apply_clause_to_state state outcome root clause =
  let filesystem = {clause; root; cwd=state.filesystem.cwd} in
  let state' = { state with filesystem } in
  let result = outcome_to_bool outcome in
  state', result

let apply_case_to_state state root case : (state * bool) list =
  (* Add the spec to the current clause *)
  Clause.add_to_sat_conj case.spec state.filesystem.clause
  (* Quantify over the old state root *)
  |> List.map (Clause.quantify_over state.filesystem.root)
  |> List.flatten
  |> List.map (apply_clause_to_state state case.outcome root)

type specifications = Path.t -> Var.t -> Var.t -> case list

let under_specifications : specifications -> state -> (state * bool) list =
  fun spec state ->
    let new_root = Var.fresh ~hint:(Var.to_string state.filesystem.root) () in
    spec state.filesystem.cwd state.filesystem.root new_root
    |> List.map (apply_case_to_state state new_root)
    |> List.flatten
