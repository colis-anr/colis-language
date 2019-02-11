open Constraints
open SymbolicInterpreter__Filesystem
open SymbolicInterpreter__State
open Semantics__Buffers

type utility = state -> (bool state_result) list

let print_line msg state =
  let open Semantics__Buffers in
  let stdout = Stdout.(output msg state.stdout |> newline) in
  {state with stdout}

let print_utility_descr result msg state =
  if String.equal msg "" then
    state
  else
    let prefix = match result with
      | Result true ->  "[DBG] "
      | Result false -> "[ERR] "
      | Incomplete ->   "[INCOMPLETE] "
    in
    print_line (prefix^msg) state

type case = {
  result : bool result;
  spec : Clause.t;
  descr : string;
  stdout : Stdout.t ;
}

let success_case ~descr ?(stdout=Stdout.empty) spec =
  { result = Result true ; stdout ; descr; spec }

let error_case ~descr ?(stdout=Stdout.empty) spec =
  { result = Result false ; stdout ; descr ; spec }

let failure_case ~descr ?(stdout=Stdout.empty) spec =
  { result = Incomplete ; stdout ; descr ; spec }

let quantify_over_intermediate_root state conj =
  if BatOption.eq ~eq:Var.equal state.filesystem.root0 (Some state.filesystem.root) then
    [conj]
  else
    Clause.quantify_over state.filesystem.root conj

let apply_output_to_state (state : state) stdout =
  { state with stdout = Stdout.concat state.stdout stdout }

(* Create the corresponding filesystem, update the state and create corresponding
    result **)
let apply_clause_to_state state case root clause =
  let filesystem = {state.filesystem with clause; root} in
  let state' =
    { state with filesystem }
    |> print_utility_descr case.result case.descr
  in
  state', case.result

let apply_case_to_state state root case =
  let state = apply_output_to_state state case.stdout in
  (* Add the case specification to the current clause *)
  Clause.add_to_sat_conj case.spec state.filesystem.clause
  |> List.map (quantify_over_intermediate_root state)
  |> List.flatten
  |> List.map (apply_clause_to_state state case root)

type specifications = cwd:Path.t -> root:Var.t -> root':Var.t -> case list

let under_specifications spec state =
  let new_root = Var.fresh ~hint:(Var.hint state.filesystem.root) () in
  let cases = spec ~cwd:state.filesystem.cwd ~root:state.filesystem.root ~root':new_root in
  List.map (apply_case_to_state state new_root) cases |> List.flatten
